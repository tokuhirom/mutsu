Log::Async
==========
Thread-safe asynchronous logging using supplies.

[![SparrowCI](https://ci.sparrowhub.io/project/gh-bduggan-raku-log-async/badge)](https://ci.sparrowhub.io)
[![Actions Status](https://github.com/bduggan/raku-log-async/actions/workflows/linux.yml/badge.svg)](https://github.com/bduggan/raku-log-async/actions/workflows/linux.yml)
[![Actions Status](https://github.com/bduggan/raku-log-async/actions/workflows/macos.yml/badge.svg)](https://github.com/bduggan/raku-log-async/actions/workflows/macos.yml)


Synopsis
========

This module can be used in a few different ways:

```raku
use Log::Async <trace>;           # send all logs to stderr
use Log::Async <cli trace>;       # send logs to stder, parse command line options
use Log::Async <cli trace color>; # in color
use Log::Async <warning color>;   # only send warning, error and fatal to stderr
use Log::Async <info>;            # only send info, warn, error and fatal
```

Or don't import anything and set the destinations and levels to log yourself:

```raku
use Log::Async;
logger.send-to($*OUT);

trace 'how';
debug 'now';
info 'brown';
warning 'cow';
error 'wow';
fatal 'ow';

(start debug 'one')
  .then({ debug 'two' });
(start debug 'buckle')
  .then({ debug 'my shoe' });
sleep 1;

my $when = now + 1;

for ^100 {
     Promise.at($when)
         .then({ debug "come together"})
         .then({ debug "right now"})
         .then({ debug "over me"});
}

logger.send-to("/var/log/hal.errors", :level(ERROR));
error "I'm sorry Dave, I'm afraid I can't do that";
```

Description
===========

`Log::Async` provides asynchronous logging using
the supply and tap semantics of Raku.  Log messages
are emitted asynchronously to a supply.  Taps are
only executed by one thread at a time.

By default a single tap is created which prints the timestamp,
level and message to stdout.

Exports
=======

**trace, debug, info, warning, error, fatal**: each of these
asynchronously emits a message at that level.

**enum Loglevels**: TRACE DEBUG INFO WARNING ERROR FATAL

**class Log::Async**: Does the real work.

**sub logger**: return or create a logger singleton.

**sub set-logger**: set a new logger singleton.

Log::Async Methods
==========

### add-tap(Code $code,:$level,:$msg)
```raku
my $tap = logger.add-tap({ say $^m<msg> ~ '!!!!!' },  :level(FATAL));
logger.add-tap({ $*ERR.say $^m<msg> },      :level(DEBUG | ERROR));
logger.add-tap({ say "# $^m<msg>"},          :level(* < ERROR) );
logger.add-tap({ say "meow: " ~ $^m<msg> }, :msg(rx/cat/));
logger.add-tap(-> $m { say "thread { $m<THREAD>.id } says $m<msg>" });
logger.add-tap(-> $m { say "$m<when> {$m<frame>.file} {$m<frame>.line} $m<level>: $m<msg>" });
logger.add-tap(-> $m { say "{ $m<when>.utc } ($m<level>) $m<msg>",
    :level(INFO..WARNING) });
```

Add a tap, optionally filtering by the level or by the message.
`$code` receives a hash with the keys `msg` (a string), `level` (a
Loglevel), `when` (a DateTime), `THREAD` (the caller's $\*THREAD),
`frame` (the current callframe), and possibly `ctx` (the context, see below).

`$level` and `$msg` are filters: they will be smartmatched against
the level and msg keys respectively.

`add-tap` returns a tap, which can be sent to `remove-tap` to turn
it off.

###  remove-tap($tap)

```raku
logger.remove-tap($tap)
```
Closes and removes a tap.

### send-to(Str $filename, Code :$formatter, |args)
```raku
send-to(IO::Handle $handle)
send-to(IO::Path $path)
logger.send-to('/tmp/out.log');
logger.send-to('/tmp/out.log', :level( * >= ERROR));
logger.send-to('/tmp/out.log', formatter => -> $m, :$fh { $fh.say: "{$m<level>.lc}: $m<msg>" });
logger.send-to($*OUT,
  formatter => -> $m, :$fh {
    $fh.say: "{ $m<frame>.file } { $m<frame>.line } { $m<frame>.code.name }: $m<msg>"
  });
```
Add a tap that prints timestamp, level and message to a file or filehandle.
`formatter` is a Code argument which takes `$m` (see above), as well as
the named argument `:$fh` -- an open filehandle for the destination.

Additional args (filters) are sent to add-tap.

### close-taps
```raku
logger.close-taps
```
Close all the taps.

### done
```raku
logger.done
```
Tell the supplier it is done, then wait for the supply to be done.
This is automatically called in the END phase.

### untapped-ok
```raku
logger.untapped-ok = True
```
This will suppress warnings about sending a log message before any
taps are added.

Context
=======
To display stack trace information, logging can be initialized with `add-context`.
This sends a stack trace with every log request (so may be expensive).  Once `add-context`
has been called, a `ctx` element will be passed which is a `Log::Async::Context`
object.  This has a `stack` method which returns an array of backtrace frames.

```raku
logger.add-context;
logger.send-to('/var/log/debug.out',
  formatter => -> $m, :$fh {
    $fh.say: "file { $m<ctx>.file}, line { $m<ctx>.line }, message { $m<msg> }"
    }
  );
logger.send-to('/var/log/trace.out',
  formatter => -> $m, :$fh {
      $fh.say: $m<msg>;
      $fh.say: "file { .file}, line { .line }" for $m<ctx>.stack;
    }
  );
```

A custom context object can be used as an argument to add-context.  This
object should have a `generate` method. `generate` will be called to
generate context whenever a log message is sent.

For instance:
```raku
my $context = Log::Async::Context.new but role {
  method generate { ... }
  method custom-method { ... }
  };
logger.add-context($context);

# later
logger.add-tap(-> $m { say $m.ctx.custom-method } )

```

More Examples
========


### Send debug messages to stdout.
```raku
logger.send-to($*OUT,:level(DEBUG));
```

### Send warnings, errors, and fatals to a log file.

```raku
logger.send-to('/var/log/error.log',:level(* >= WARNING));
```

### Add a tap that prints the file, line number, message, and utc timestamp.

```raku
logger.send-to($*OUT,
  formatter => -> $m, :$fh {
    $fh.say: "{ $m<when>.utc } ({ $m<frame>.file } +{ $m<frame>.line }) $m<level> $m<msg>"
  });
trace 'hi';

# output:
2017-02-20T14:00:00.961447Z (eg/out.raku +10) TRACE hi
```


Caveats
=======
Because messages are emitted asynchronously, the order in which
they are emitted depends on the scheduler.  Taps are executed
in the same order in which they are emitted.  Therefore timestamps
in the log might not be in chronological order.

Author
======
Brian Duggan

Contributors
============
Bahtiar Gadimov

Curt Tilmes

Marcel Timmerman

Juan Julián Merelo Guervós

Slobodan Mišković
