OpenCensus repl
===============

Tutorial app to demo OpenCensus metrics functionality

Requirements
------------
- Erlang 20.0 or above
- Rebar3
- [Prometheus](https://prometheus.io/docs/introduction/first_steps/) as our choice of metrics backend: we are picking it because it is free, 
  open source and easy to setup

Build
-----
```
$ rebar3 compile
```

Running the example
-------------------

This step involves running the tutorial application in one terminal 
and then Prometheus itself in another terminal.
```
rebar3 shell --sname repl@localhost

(repl@localhost)1> repl:run().
```

To enable Prometheus to scrape from your application, we have to point it towards the tutorial application 
whose server is running on “localhost:8081”.

Running Prometheus
==================
```
prometheus --config.file=promconfig.yaml
```

Viewing your metrics
====================
With the above you should now be able to navigate to the Prometheus UI at http://localhost:9090