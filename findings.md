# grpc-haskell review findings

Scope: the C shim (`core/cbits`), `Network.GRPC.Unsafe.*`, `Network.GRPC.LowLevel.*` and `Network.GRPC.HighLevel.*`.

This began as a static review. gRPC API contracts were checked against the 1.82.1 headers; claims about gRPC core internals are from memory and are marked "unverified". Only #1 has been reproduced by a test.

## Status

Finding #1 is fixed on branch `riz0id/completion-queue`: each completion queue is now read by a single dispatcher thread using `next`, and waiters block on an `MVar` (`core/src/Network/GRPC/LowLevel/CompletionQueue/Internal.hs`). The regression tests are `testIdleStreams` in `core/tests/LowLevelTests.hs`.

The same change also addresses, by construction and without dedicated tests:

- **#3 and #4:** memory handed to gRPC is now owned by the operation's completion callback, so it is no longer freed while gRPC can still write to it.
- **#13:** `dispatchLoop` stops when the server begins shutting down.
- **Uninterruptible waits (low severity):** blocked ops and waits for new calls can now be interrupted.

Later changes on the same branch fix part of #5 (the unsafe import), two bullets of #10 (credentials and callback wrappers) and the low-severity `forkServer` race; each is marked below.

Everything else below is unchanged.

## High severity

### 1. At most 6 blocking ops per completion queue, so idle streams can stall a server or client

**Fixed.** Reproduced before the fix: with 6 idle streams a unary call failed with `StatusDeadlineExceeded` after its 5-second deadline, while with 5 it succeeded. The description below is of the old code.

- Every `runOps` holds a pluck slot for the whole time it blocks (`core/src/Network/GRPC/LowLevel/Op.hs:239`, `core/src/Network/GRPC/LowLevel/CompletionQueue/Internal.hs:86`), and `GRPC_MAX_COMPLETION_QUEUE_PLUCKERS` is 6.
- All server calls share one `serverCallCQ`, and all client calls share one `clientCQ`.
- Six handlers blocked in `streamRecv` on idle client or bidi streams make every other op wait in STM `retry`, including sends on unrelated calls. Six idle streaming clients are enough to stall the server.

### 2. `stopServer` waits for in-flight calls before cancelling them, then throws

- `core/src/Network/GRPC/LowLevel/Server.hs:235-265` plucks the shutdown tag for 30s before calling `grpcServerCancelAllCalls` or killing handlers.
- The header says that tag fires only "when there are no more calls being serviced".
- Any call still running after 30s gives `error "Failed to stop server."` from inside `bracket`'s release, and the server and both CQs are never destroyed.

### 3. Use-after-free when a thread waiting for an unregistered call is killed

- `core/src/Network/GRPC/LowLevel/CompletionQueue/Unregistered.hs:55-70` plucks in 1s slices "for interruptibility".
- An async exception makes `with allocs` free the call pointer, metadata array and call details while the request is still pending in core.
- Core later writes into that freed memory when a call arrives or when shutdown fails the request (unverified for the shutdown path).

### 4. `runOps` can free op contexts while the batch is still in flight

- After `startBatch` succeeds (`core/src/Network/GRPC/LowLevel/Op.hs:232-245`), `pluck` returns `Left GRPCIOShutdown` without plucking if the CQ began shutting down in between.
- `mask_` does not protect the wait for a pluck slot either, because STM `retry` is interruptible; with more than 6 pluckers an async exception lands there.
- In both cases the teardown frees buffers gRPC will still write to.

### 5. Client metadata plugin violates the gRPC callback contract

- `metadata_dispatcher` (`core/cbits/grpc_haskell.c:509-519`) returns 0 (async), but `convertMetadataCreate` (`core/src/Network/GRPC/Unsafe/Security.chs:388-396`) invokes `cb` synchronously on the same thread.
- `credentials.h` says `cb` "MUST be invoked in a different thread" in that case.
- **Fixed:** `grpc_call_start_batch` was imported `unsafe`, and core runs the plugin from inside it on an already-connected channel, re-entering Haskell from an unsafe call. Reproduced before the fix: the second request on a plugin client hung. The import is now safe; the regression test is `testAuthMetadataRepeated` in `core/tests/LowLevelTests.hs`.
- The callback-contract problem in the first two bullets is still open.

### 6. Possible NULL dereference from a remote client on registered methods

- `extractPayload` (`core/src/Network/GRPC/LowLevel/Call.hs:57-61`) passes the payload straight to `copyByteBufferToByteString` with no null check.
- If a client half-closes a Normal or ServerStreaming call without sending a message, core likely delivers a NULL payload (unverified), which would segfault the server.
- The op-based path does check for NULL (`core/src/Network/GRPC/LowLevel/Op.hs:165`).

## Medium severity

### 7. Bidi client handler receives the wrong metadata

`src/Network/GRPC/HighLevel/Client.hs:146` passes `meta`, the client's own request metadata, where the handler expects the server's initial metadata; the real getter `_m` is discarded.

### 8. `MaxReceiveMessageLength` and `MaxMetadataSize` truncate

`core/src/Network/GRPC/Unsafe/ChannelArgs.chs:73-78` clamps to `maxBound :: Int`, but the value is then marshalled to a C `int`. For example, `2^32 + 5` becomes a 5-byte limit and `2^31` becomes negative.

### 9. Batch failures are reported as timeouts and then swallowed

`runOps` reports any unsuccessful batch as `GRPCIOTimeout` (its `opFailure`, in `core/src/Network/GRPC/LowLevel/Op.hs`), and `handleCallError` ignores `GRPCIOTimeout` (`src/Network/GRPC/HighLevel/Server.hs:192`), so real failures are never logged.

### 10. Memory leaks on hot paths

- Metadata key and value slices are never unref'd; `metadata_free` frees only the array (`core/cbits/grpc_haskell.c:182-191`, used from `core/src/Network/GRPC/LowLevel/Op.hs:108,112` and `core/src/Network/GRPC/Unsafe/Metadata.chs:111`). This leaks every key or value over 23 bytes, per call, assuming core does not take ownership (unverified).
- `grpc_channel_create_call_` never unrefs `method_slice` or `host_slice` (`core/cbits/grpc_haskell.c:42-46`); the header says they need only live through the call.
- **Fixed:** `addMetadataCreds` never released the call credentials or the composite credentials. It is replaced by `withMetadataCreds` (`core/src/Network/GRPC/LowLevel/Client.hs`), which releases both.
- **Fixed:** the plugin and processor structs and their `FunPtr` wrappers were never freed. The structs are now freed once gRPC has copied them, and gRPC frees each wrapper through the struct's `destroy` callback (`core/cbits/grpc_haskell.c`). The tests are `testAuthPluginFreed` and `testAuthProcessorFreed`.

### 11. A client can trigger `error` in the unregistered unary handler

A call closed without a message yields `OpRecvMessageResult Nothing`, which hits `error "impossible pattern match"` (`core/src/Network/GRPC/LowLevel/Server/Unregistered.hs:138`).

### 12. Unknown methods are handled incorrectly

`src/Network/GRPC/HighLevel/Server/Unregistered.hs:70` waits for a request message first and then replies `StatusNotFound`; the gRPC spec calls for `UNIMPLEMENTED`.

### 13. `dispatchLoop` busy-spins during shutdown

`forever $ withServerCallAsync` (`src/Network/GRPC/HighLevel/Server/Unregistered.hs:37`) retries immediately on `Left`. Once shutdown starts, each request fails at once, so the thread spins until it is killed, up to the 30s in item 2.

### 14. Decode failures and handler exceptions never send a status

- `convertServerHandler` throws `GRPCIODecodeError` (`src/Network/GRPC/HighLevel/Server.hs:73`), which is re-wrapped as `GRPCIOHandlerException`; the call is just unref'd, so the client sees a cancellation instead of `INVALID_ARGUMENT` or `INTERNAL`.
- In `convertServerWriterHandler` (`src/Network/GRPC/HighLevel/Server.hs:124`) the throw is lazy and fires wherever user code first forces the request.

## Low severity

- **Wrong count in `metadata_array_set_metadata`:** it uses `sizeof(meta)`, the pointer size, as the element count (`core/cbits/grpc_haskell.c:351`). It is currently unused.
- **Garbage constants:** `argEnableCensus`, `argMaxConcurrentStreams` and `argMaxMessageLength` apply `#const` to string macros (`core/src/Network/GRPC/Unsafe/Constants.hsc:9-16`), so their values are meaningless.
- **Authority ignored:** the unregistered client path does not honour `clientAuthority`; the registered path does (`core/src/Network/GRPC/LowLevel/Client/Unregistered.hs:45` vs `core/src/Network/GRPC/LowLevel/Client.hs:151`).
- **Uninterruptible waits:** `runOps` blocks under `mask_` in a foreign call, so `timeout` or `killThread` cannot interrupt a recv on a deadline-less stream. The registered `serverRequestCall` plucks with an infinite deadline and has the same problem.
- **Shutdown result dropped:** `withCompletionQueue` discards the `Either` from `shutdownCompletionQueue`, and that function waits without a timeout for pluckers to reach zero.
- **`forkServer` race (fixed in `7ce9298`):** a thread forked between the `serverShuttingDown` check and the set insert was missed by the kill sweep. `forkServer` now checks and registers in one transaction (`core/src/Network/GRPC/LowLevel/Server.hs`).
- **Empty-metadata leak:** `op_send_initial_metadata_empty` leaks its `malloc(0)`; it is unused.

## Suggested order

Of the high-severity items, #2, #5 and #6 remain. #2 is a reordering inside `stopServer`.
