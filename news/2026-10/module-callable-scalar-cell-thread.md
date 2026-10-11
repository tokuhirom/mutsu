# A Sub held in a module-scope scalar is shared with worker threads

`HTTP::Server::Middleware::JSON`'s `json-error` reassigns a module-scope
`my $err = sub {...}`. The handler closure runs on a worker thread, which kept
reading the stale spawn-time `Sub`, so the custom-error-handler assertion of
`t/00-basic.t` failed (4 of 5 passed).

The cause was `box_decl_local_cell_any_sigil` refusing a shared cell for any
`Sub` value that was not held in a `&` variable. Without the cell the write never
reached the worker's name lane. A `$` scalar holding a `Sub` is now boxed like
any other scalar value. Pinned by
`t/concurrency/thread-lock/module-scalar-callable-thread-visible.t`; the
distribution's suite now passes 5 of 5 by direct run.
