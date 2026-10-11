# A thread sees a module lexical reassigned after it started (pinned)

[#11697](https://github.com/tokuhirom/mutsu/issues/11697) reported a `my`
variable of a `module { ... }` block that one of the module's subs reassigned
after a thread had started. Code already running in that thread kept seeing the
old value. HTTP::Server::Middleware::JSON 0.0.3's `t/00-basic.t` failed its
fifth test because of it: `json-error` installs a custom handler while the
HTTP::Server::Async server thread is running.

On current `main` the thread sees the new value. The issue's own repro still
prints the wrong thing, but for an unrelated reason: it reads the channel with
`for $c.list`, which on mutsu delivers an item only when the next `send`
arrives (#12727). Rewritten with `.receive`, the repro prints `default 1` /
`custom 2`, as rakudo does. HTTP::Server::Middleware::JSON's `t/00-basic.t`
passes 5/5 on mutsu, as it does on rakudo.

`t/concurrency/thread-lock/module-lexical-reassigned-seen-by-thread.t` pins
the behaviour: a module-level code value, scalar and hash written by the main
thread and by a second thread, all read from a thread that was already running.
