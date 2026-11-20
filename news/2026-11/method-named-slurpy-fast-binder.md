# A method with its own `*%h` binds on the fast binder

Before this change, a method that declares its own named slurpy sent every
call to the general binder. Examples are a constructor
`method new(:$a, *%rest)` and AttrX::Mooish's
`AttrProxy.new(:$FETCH, :$STORE, *%attrinit)`. Such a method gets no implicit
`*%_`.

The fast binder now fills a plain `*%h`, one with no type, `where`,
sub-signature or trait, with the named arguments that no explicit named
parameter consumes. raku keeps a variable argument's container in the hash:
`%h<a> = 7` assigns the caller's `$v`, and `$!x := %h<a>` aliases it. So each
leftover argument is classified the way a named `is raw` argument is:

- A value is stored as it is.
- A shared cell is stored as that cell.
- A caller lexical still held as a plain value has its slot promoted to a
  shared cell while the caller's frame is current. This happens after the
  call's deferral frame has recorded the arguments, so `callsame` forwards the
  caller's values.

A `Proxy` argument, or a source the classification cannot settle, keeps the
general binder.

Per call, measured with a callgrind slope on a profiling build:

- `K.s2(FETCH => &f, STORE => &s)` for `method s2(:$FETCH, :$STORE, *%attrinit)`
  went from 48.1k to 35.5k instructions.
- With two more named arguments, one of them a variable that is promoted, it
  went from 62.2k to 52.8k.

On the AttrX::Mooish t/090-concurrent probe, `AttrProxy.new` leaves the general
binder, and a round went from 4,796,501 to 4,782,972 instructions (-0.28%).
