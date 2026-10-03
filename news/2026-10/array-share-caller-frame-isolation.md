# A recursive call no longer overwrites the caller's loop variable

Assigning a loop parameter that holds an Array or Hash to a scalar
(`my $prev = $item`, `$prev-item = $item`) promotes the source to a shared
cell, and that promotion also patched every saved call frame whose
environment held a variable *of the same name*, so the sharing survives an env
restore. A recursive call has its own `-> $idx, $item` parameter, so the inner
call's promotion overwrote the outer iteration's `$item` with the inner item:
Template::Jinja2's recursive `for` reported the wrong `loop.previtem`. The
saved-frame propagation now only touches a frame whose entry is the very
binding being promoted (a closure's view of an enclosing lexical), reading the
binding from the local slot when the name has one (#11304).
