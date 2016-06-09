####erlmars

This is a basic application that i wrote while i was learning erlang.

It will do scheduled runs to fetch mars weather data and aggregate it in a postgresql database.

This app requires a couple of env vars:

 - PGUSER, PGPASS, PGDBNAME, PGURL (postgresql configuration vars)

The logs of the app will be served under /var/log/erlmars


```
/src
  /erlmars.app.src : application information file for OTP
  /erlmars_app.erl : base module for the Erlang application behavior
  /erlmars_config.erl : configuration interface for your application
  /erlmars_sup.erl : OTP supervisor for the application
  /erlmars_resource.erl : a simple example Webmachine resource
/priv
  /www : a convenient place to put your static web content
```


### Build the application:

```
$ rebar3 compile
```

### Start up the application:
```
$ rebar3 release
...
$ ./_build/default/rel/erlmars/bin/erlmars console
```

*or*

```
$ rebar3 shell
```

### Change the basic application:
* edit src/erlmars_resource.erl

### Add some new resources:
* edit src/YOUR_NEW_RESOURCE.erl
* edit src/erlmars_config.erl's `dispatch/0` function

### On the fly editing

We're using `sync` now to do on the fly compilation of resources.

Once you're in a console, just type `sync:go().` and it will recompile
your files on the fly, but you'll have to use the dev profile:

```
$ rebar3 as dev shell
```
