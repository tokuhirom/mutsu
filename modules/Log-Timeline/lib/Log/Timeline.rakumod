use Log::Timeline::Model;
use Log::Timeline::Output::CBORSequence;
use Log::Timeline::Output::JSONLines;
use Log::Timeline::Output::Socket;
use Log::Timeline::Raku::Setup;

class Log::Timeline {
    # Check if an output of some kind is set up for logging.
    method has-output() {
        PROCESS::<$LOG-TIMELINE-OUTPUT>.defined
    }
}

# The mainline of a module runs once. We use this to do the setup phase of
# the desired output, based on environment variables.
with %*ENV<LOG_TIMELINE_SERVER> {
    setup-raku-events();
    when /^ \d+ $/ {
        PROCESS::<$LOG-TIMELINE-OUTPUT> = Log::Timeline::Output::Socket.new(port => +$/);
    }
    when /^ (.+) ':' (\d+) $/ {
        PROCESS::<$LOG-TIMELINE-OUTPUT> = Log::Timeline::Output::Socket.new(host => ~$0, port => +$1);
    }
    default {
        die "Expected LOG_TIMELINE_SERVER to contain a port number or host:port";
    }
}
orwith %*ENV<LOG_TIMELINE_JSON_LINES> {
    setup-raku-events();
    PROCESS::<$LOG-TIMELINE-OUTPUT> = Log::Timeline::Output::JSONLines.new(path => .IO);
}
orwith %*ENV<LOG_TIMELINE_CBOR_SEQUENCE> {
    setup-raku-events();
    PROCESS::<$LOG-TIMELINE-OUTPUT> = Log::Timeline::Output::CBORSequence.new(path => .IO);
}

END try .close with PROCESS::<$LOG-TIMELINE-OUTPUT>;

=begin pod

=head1 NAME

Log::Timeline - Log tasks with start/end periods and phases, as well as individual events

=head1 DESCRIPTION

When building an application with many ongoing, potentially overlapping,
tasks, we may find ourselves wishing to observe what is going on. We'd
like to log, but with a focus on things that happen over time rather
than just individual events. The C<Log::Timeline> module provides a means
to do that.

As well as annotating your own applications with C<Log::Timeline> tasks
and events, the module can provide various events relating to the Raku
standard library, and has already been integrated in some modules, such
as C<Cro>.

=head1 Key features

=item Log tasks with start and end times

=item Log individual events

=item Tasks and events can be associated with an enclosing parent task

=item Include data (keys mapped to values) with the logged tasks and
events

=item Have data logged to a file (JSON or CBOR), or exposed over a
socket

=item Visualize task timelines in L<Comma|https://commaide.com/>

=item Support by L<Cro|https://cro.services/>, to offer insight into
client and server request processing pipelines

=item Enable logging of various Raku standard library events: the
start and end times of C<start> blocks and C<await> statements,
socket connections, files being open, and more

Planned:

=item Introspect what tasks and events a given distribution can log

=item Log tasks indicating when GC happens

=item Turn on/off what is logged at runtime (socket mode only)

=head1 Turning on Raku built-ins logging

Set the `LOG_TIMELINE_RAKU_EVENTS` environment variable to a comma-separated
list of events to log. For example:


=begin code

LOG_TIMELINE_RAKU_EVENTS=file,thread,socket,process,start,await

=end code

The events available are:

=item C<await> - logs tasks showing time spent in an `await`

=item C<file> - logs tasks showing the time files are open

=item C<process> - logs tasks showing the time that a sub-process
is running (the logging is done on C<Proc::Async>, which covers
everything since the synchronous process API is a wrapper around
the asynchronous one)

=item C<socket> - logs a task when a listening asynchornous socket
is listening, and child tasks for each connection it receives; for
connections, the connection is logged along with a child task for
the time taken for the initial connection establishment

=item C<start> - logs tasks showing the time that C<start> blocks
are "running" (however, they may within that time be suspended due
to an C<await>)

=item C<thread> - logs creation of Raku C<Thread>s. These are
typically created by the thread pool when using Raku's high-level
concurrency features.

=head1 Providing tasks and events in a distribution

Providing tasks and events in your application involves the
following steps:

=item 1. Make sure that your C<META6.json> contains a C<depends>
entry for C<Log::Timeline>

=item 2. Create one or more modules whose final name part is
C<LogTimelineSchema>, which declares the available tasks and
events. This will be used for tools to introspect the available
set of tasks and events that might be logged, and to provide
their metadata

=item 3. Use the schema module and produce timeline tasks and
events in your application code

=head2 The schema module

Your application or module should specify the types of tasks and
events it wishes to log. These are specified in one or more modules,
which should be registered in the C<provides> section of the
C<META6.json> file. The B<module name's final component> should be
C<LogTimelineSchema>. For example, C<Cro::HTTP> provides
C<Cro::HTTP::LogTimelineSchema>.  You may provide more than one of
these per distribution.

Every task or event has a 3-part name:

=item B<Module> - for example, C<Cro HTTP>

=item B<Category> - for example, C<Client> and C<Server>

=item B<Name> - for example, C<HTTP Request>

These are specified when doing the role for the event or task.

To declare an event (something that happens at a single point in
time), do the C<Log::Timeline::Event> role. To declare a task
(which happens over time) do the C<Log::Timeline::Task> role.

=begin code :lang<raku>

unit module MyApp::Log::LogTimelineSchema;
use Log::Timeline;

class CacheExpired
  does Log::Timeline::Event['MyApp', 'Backend', 'Cache Expired'] { }

class Search
  does Log::Timeline::Task['MyApp', 'Backend', 'Search'] { }

=end code

=head2 Produce tasks and events

Use the module in which you placed the events and/or tasks you
wish to log.

=begin code :lang<raku>

use MyApp::Log::LogTimelineSchema;

=end code

To log an event, simply call the C<log> method:

=begin code :lang<raku>

MyApp::Log::LogTimelineSchema::CacheExpired.log();

=end code

Optionally passing extra data as named arguments:

=begin code :lang<raku>

MyApp::Log::LogTimelineSchema::CacheExpired.log(:$cause);

=end code

To log a task, also call C<log>, but pass a block that will execute
the task:

=begin code :lang<raku>

MyApp::Log::LogTimelineSchema::Search.log: -> {
    # search is performed here
}

=end code

Named parameters may also be passed to provide extra data:

=begin code :lang<raku>

MyApp::Log::LogTimelineSchema::Search.log: :$query -> {
    # search is performed here
}

=end code

=head1 Collecting data

=head2 Logging to a file in JSON lines format

Set the C<LOG_TIMELINE_JSON_LINES> environment variable to the name
of a file to log to. Each line is an object with the following keys:

=item C<m> - module

=item C<c> - category

=item C<n> - name

=item C<t> - timestamp

=item C<d> - data (an object with any extra data)

=item C<k> - kind (0 = event, 1 = task start, 2 = task end)

A task start (kind 1) and task end (2) will also have:

=item C<i> - a unique ID for the task, starting from 1, to allow
starts and ends to be matched up

An event (kind 0) or task start (kind 1) may also have:

=item C<p> - the parent task ID

=head2 Logging to a file as a CBOR sequence

Set the C<LOG_TIMELINE_CBOR_SEQUENCE> environment variable to the
name of a file to log into. The schema matches that of the JSON
lines output.

=head2 Socket logging

Set the C<LOG_TIMELINE_SERVER> environment variable to either:

=item A port number, to bind to C<localhost> on that port

=item A string of the form C<host:port>, e.g. C<127.0.0.1:5555>

B<Warning:> Don't expose the socket server to the internet
directly; there is no authentication or encryption. If really
wishing to expose it, bind it to a local port and then use an
SSH tunnel.

=head2 Handshake

Upon connection the client C<must> send a JSON line consisting
of an object that includes the keys:

=item C<min> - the minimum protocol version that the client understands

=item C<max> - the maximum protocol version that the client understands

The client I<may> include other keys in the object speculatively (for
example, if protocol version 3 supports a key "foo", but it speaks
anything from 1 to 3, then it may include the key "foo", knowing that
a previous version of the server will simply ignore it).

In response to this, the server I<must> send a JSON line consisting
of an object that includes I<at most one of the following>:

=item C<ver> - the version number of the protocol that the server will
speak, if it is understands any of the versions in the range the client
proposed

=item C<err> - an error string explaining why it will not accept the
request

In the case of sending an C<err>, the server I<should> close the
connection.

If the initial communication from the client to the server:

=item Does not start with a `{`

=item Does not reach a complete line within 1 megabyte of data

Then the server may send a JSON line with an object containing C<err>
and then close the connection.

=head2 Protocol version 1

No additional configuration keys from the client are recognized in
this version of the protocol.

Beyond the initial handshake line, the client should not send anything
to the server. The client may close the connection at any time.

The server sends JSON lines to the client. This lines are the same as
specified for the JSON lines file format.

=head2 Checking if logging is active

Call C<Log::Timeline.has-output> to see if some kind of logging output
is set up in this process, This is useful for avoiding introducing
logging if it will never take place.

=head1 AUTHOR

Jonathan Worthington

=head1 COPYRIGHT AND LICENSE

Copyright 2019 - 2024 Jonathan Worthington

Copyright 2024 Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
