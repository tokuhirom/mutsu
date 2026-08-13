[![Actions Status](https://github.com/raku-community-modules/Log-Timeline/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/Log-Timeline/actions) [![Actions Status](https://github.com/raku-community-modules/Log-Timeline/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/Log-Timeline/actions) [![Actions Status](https://github.com/raku-community-modules/Log-Timeline/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/Log-Timeline/actions)

NAME
====

Log::Timeline - Log tasks with start/end periods and phases, as well as individual events

DESCRIPTION
===========

When building an application with many ongoing, potentially overlapping, tasks, we may find ourselves wishing to observe what is going on. We'd like to log, but with a focus on things that happen over time rather than just individual events. The `Log::Timeline` module provides a means to do that.

As well as annotating your own applications with `Log::Timeline` tasks and events, the module can provide various events relating to the Raku standard library, and has already been integrated in some modules, such as `Cro`.

Key features
============

  * Log tasks with start and end times

  * Log individual events

  * Tasks and events can be associated with an enclosing parent task

  * Include data (keys mapped to values) with the logged tasks and events

  * Have data logged to a file (JSON or CBOR), or exposed over a socket

  * Visualize task timelines in [Comma](https://commaide.com/)

  * Support by [Cro](https://cro.services/), to offer insight into client and server request processing pipelines

  * Enable logging of various Raku standard library events: the start and end times of `start` blocks and `await` statements, socket connections, files being open, and more

Planned:

  * Introspect what tasks and events a given distribution can log

  * Log tasks indicating when GC happens

  * Turn on/off what is logged at runtime (socket mode only)

Turning on Raku built-ins logging
=================================

Set the `LOG_TIMELINE_RAKU_EVENTS` environment variable to a comma-separated list of events to log. For example:

    LOG_TIMELINE_RAKU_EVENTS=file,thread,socket,process,start,await

The events available are:

  * `await` - logs tasks showing time spent in an `await`

  * `file` - logs tasks showing the time files are open

  * `process` - logs tasks showing the time that a sub-process is running (the logging is done on `Proc::Async`, which covers everything since the synchronous process API is a wrapper around the asynchronous one)

  * `socket` - logs a task when a listening asynchornous socket is listening, and child tasks for each connection it receives; for connections, the connection is logged along with a child task for the time taken for the initial connection establishment

  * `start` - logs tasks showing the time that `start` blocks are "running" (however, they may within that time be suspended due to an `await`)

  * `thread` - logs creation of Raku `Thread`s. These are typically created by the thread pool when using Raku's high-level concurrency features.

Providing tasks and events in a distribution
============================================

Providing tasks and events in your application involves the following steps:

  * 1. Make sure that your `META6.json` contains a `depends` entry for `Log::Timeline`

  * 2. Create one or more modules whose final name part is `LogTimelineSchema`, which declares the available tasks and events. This will be used for tools to introspect the available set of tasks and events that might be logged, and to provide their metadata

  * 3. Use the schema module and produce timeline tasks and events in your application code

The schema module
-----------------

Your application or module should specify the types of tasks and events it wishes to log. These are specified in one or more modules, which should be registered in the `provides` section of the `META6.json` file. The **module name's final component** should be `LogTimelineSchema`. For example, `Cro::HTTP` provides `Cro::HTTP::LogTimelineSchema`. You may provide more than one of these per distribution.

Every task or event has a 3-part name:

  * **Module** - for example, `Cro HTTP`

  * **Category** - for example, `Client` and `Server`

  * **Name** - for example, `HTTP Request`

These are specified when doing the role for the event or task.

To declare an event (something that happens at a single point in time), do the `Log::Timeline::Event` role. To declare a task (which happens over time) do the `Log::Timeline::Task` role.

```raku
unit module MyApp::Log::LogTimelineSchema;
use Log::Timeline;

class CacheExpired
  does Log::Timeline::Event['MyApp', 'Backend', 'Cache Expired'] { }

class Search
  does Log::Timeline::Task['MyApp', 'Backend', 'Search'] { }
```

Produce tasks and events
------------------------

Use the module in which you placed the events and/or tasks you wish to log.

```raku
use MyApp::Log::LogTimelineSchema;
```

To log an event, simply call the `log` method:

```raku
MyApp::Log::LogTimelineSchema::CacheExpired.log();
```

Optionally passing extra data as named arguments:

```raku
MyApp::Log::LogTimelineSchema::CacheExpired.log(:$cause);
```

To log a task, also call `log`, but pass a block that will execute the task:

```raku
MyApp::Log::LogTimelineSchema::Search.log: -> {
    # search is performed here
}
```

Named parameters may also be passed to provide extra data:

```raku
MyApp::Log::LogTimelineSchema::Search.log: :$query -> {
    # search is performed here
}
```

Collecting data
===============

Logging to a file in JSON lines format
--------------------------------------

Set the `LOG_TIMELINE_JSON_LINES` environment variable to the name of a file to log to. Each line is an object with the following keys:

  * `m` - module

  * `c` - category

  * `n` - name

  * `t` - timestamp

  * `d` - data (an object with any extra data)

  * `k` - kind (0 = event, 1 = task start, 2 = task end)

A task start (kind 1) and task end (2) will also have:

  * `i` - a unique ID for the task, starting from 1, to allow starts and ends to be matched up

An event (kind 0) or task start (kind 1) may also have:

  * `p` - the parent task ID

Logging to a file as a CBOR sequence
------------------------------------

Set the `LOG_TIMELINE_CBOR_SEQUENCE` environment variable to the name of a file to log into. The schema matches that of the JSON lines output.

Socket logging
--------------

Set the `LOG_TIMELINE_SERVER` environment variable to either:

  * A port number, to bind to `localhost` on that port

  * A string of the form `host:port`, e.g. `127.0.0.1:5555`

**Warning:** Don't expose the socket server to the internet directly; there is no authentication or encryption. If really wishing to expose it, bind it to a local port and then use an SSH tunnel.

Handshake
---------

Upon connection the client `must` send a JSON line consisting of an object that includes the keys:

  * `min` - the minimum protocol version that the client understands

  * `max` - the maximum protocol version that the client understands

The client *may* include other keys in the object speculatively (for example, if protocol version 3 supports a key "foo", but it speaks anything from 1 to 3, then it may include the key "foo", knowing that a previous version of the server will simply ignore it).

In response to this, the server *must* send a JSON line consisting of an object that includes *at most one of the following*:

  * `ver` - the version number of the protocol that the server will speak, if it is understands any of the versions in the range the client proposed

  * `err` - an error string explaining why it will not accept the request

In the case of sending an `err`, the server *should* close the connection.

If the initial communication from the client to the server:

  * Does not start with a `{`

  * Does not reach a complete line within 1 megabyte of data

Then the server may send a JSON line with an object containing `err` and then close the connection.

Protocol version 1
------------------

No additional configuration keys from the client are recognized in this version of the protocol.

Beyond the initial handshake line, the client should not send anything to the server. The client may close the connection at any time.

The server sends JSON lines to the client. This lines are the same as specified for the JSON lines file format.

Checking if logging is active
-----------------------------

Call `Log::Timeline.has-output` to see if some kind of logging output is set up in this process, This is useful for avoiding introducing logging if it will never take place.

AUTHOR
======

Jonathan Worthington

COPYRIGHT AND LICENSE
=====================

Copyright 2019 - 2024 Jonathan Worthington

Copyright 2024 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

