# An idle `IO::Socket::Async` listener wakes no thread

Each `IO::Socket::Async.listen(...).tap` used to start a `sock-async` thread of its own. The
thread polled the listener every 10 ms so it could notice a close: about 100 wake-ups a second
per idle listener, and one more thread for every listener (#12968). `nqp::asynclisten` did the
same.

Listening sockets are now watched by the `poll(2)` reactor that already serves connection
reads (#12865), the way Rakudo's event loop watches them. A connection is accepted when the
reactor reports the listener readable. A close or `nqp::cancel` wakes the reactor through its
self-pipe; the reactor then drains the backlog, drops the listener and ends the Supply. With
five idle listeners the process now has four threads, and none of them wakes in two seconds.
