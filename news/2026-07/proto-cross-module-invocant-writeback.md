# A cross-module proto-method call no longer clobbers the enclosing invocant

A plain method that delegates to an attribute whose class declares a `proto
method` in another compilation unit corrupted the *enclosing* invocant, turning it
into `Any`. The minimal shape:

```raku
# module A
class Inner { proto method field(|) {*}; multi method field($f) { ... } }
# module B
class Outer { has Inner $.inner; method poke($f) { $.inner.field($f) } }
# script
my $o = Outer.new;
$o.poke("X");        # $o is now Any!
```

`HTTP::Message.field` delegating to `HTTP::Header.field` (a cross-module `proto
method`) hit exactly this, so an `HTTP::UserAgent` request corrupted its own
request object.

The cause was in the slow proto-dispatch interception (`try_proto_method_body`).
After running the proto body it diffed the env for changed scalars to propagate
genuine caller-lexical mutations — but it treated every env key that was *new or
newly a writeback-safe scalar* as changed (`unwrap_or(true)`). The proto run leaks
bareword type objects (`Any`, `POuter`), magic vars, `__mutsu_*` keys, and even
the caller's own instance variable reset to a type object into env; recording
those as caller-var writebacks committed `Any` straight back onto the caller.

The fix requires a real *pre-existing* writeback-safe scalar whose value the proto
dispatch changed (`unwrap_or(false)`), which is the only genuine caller-lexical
mutation, and additionally excludes the invocant (`self`) and internal `__mutsu_*`
names. The enclosing `self` is also saved and restored across the proto dispatch.

Pinned by `t/proto-cross-module-invocant.t` (needs the separate
`t/lib/ProtoInvocant{Inner,Outer}.rakumod` units — a same-file proto compiles to
bytecode and never takes this path).
