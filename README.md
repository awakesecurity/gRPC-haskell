Running the tests
-----------------

In order to run the tests, you will need to have the `grpcio`, `gevent`, and
`grpcio-tools` python packages installed. You can install them using
`pip`. It is recommended that you use a python virtualenv to do this.

```
$ virtualenv path/to/virtualenv # to create a virtualenv
$ . path/to/virtual/env/bin/activate # to use an existing virtualenv
$ pip install grpcio-tools gevent
$ pip install grpcio # Need to install grpcio-tools first to avoid a versioning problem
```

