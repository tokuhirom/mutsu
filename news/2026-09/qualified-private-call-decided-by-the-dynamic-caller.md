# A qualified private call in a callback was refused by the wrong class

```raku
class Holder {
    has &.cb is rw;
    method set-cb(&c) { &!cb = &c }
    method run() { &!cb() }
}
class Owner {
    method !secret() { 'SECRET' }
    method go() {
        my $self = self;
        my $h = Holder.new;
        $h.set-cb(sub () { $self!Owner::secret() });
        $h.run;
    }
}
say Owner.new.go;
# raku:  SECRET
# mutsu: Cannot call private method 'secret' on package Owner
#        because it does not trust Holder
```

Raku decides a private call's permission **lexically** — what matters is the
class the call is written inside, not whichever class's method happens to be
running when control reaches it. mutsu read the caller off
`method_class_stack`, which names the *dynamic* caller, so a call written inside
`Owner` but reached through `Holder.run` was attributed to `Holder` and refused.

The unqualified spelling (`self!secret`) already had the right fallback:
`lexical_self_allows_private` checks the captured `self` in the running env, on
the reasoning — recorded in its own doc comment — that a closure runs with the
`self` of the scope it was written in, so an env `self` whose class is (or
inherits from) the owner marks code that was written inside that class. The
**qualified** spelling (`$o!Owner::meth`) never consulted it: both instance
dispatch paths in `src/runtime/methods_instance_ops.rs` returned the permission
error from an earlier gate, before the one that has the fallback. Adding the same
check to those two gates is the whole fix.

The check still bites where it should, and getting that boundary right took a
second pass. Reusing `lexical_self_allows_private` verbatim was **too loose**: it
accepts a `self` that merely *inherits* from the owner, which is exactly the
shape a subclass uses to reach into its parent
(`class Untrusty is Order { method try-priv { self!Order::compute_discount() } }`),
and Raku refuses that unless the parent `trusts` the subclass.
`roast/integration/advent2011-day11.t` pins it, and the first version of this fix
broke that file. The qualified path therefore gets its own strict sibling,
`lexical_self_is_private_owner`, which requires the captured `self` to be an
instance of the owner **itself** — still enough to identify code written inside
the owner, which is all the callback case needs. An untrusted class reaching in
is refused exactly as before, because its captured `self` is not the owner; an
explicit `trusts` declaration still grants access through the ordinary path.

Found reducing `Template::Jinja2` for
[#7553](https://github.com/tokuhirom/mutsu/issues/7553): its `Renderer` builds a
recursion callback as `$renderer!Renderer::render-for-items(...)` inside a closure
stored on a `LoopContext`, so every recursive `{% for %}` died with
"does not trust ... LoopContext". Pin:
`t/private-method-qualified-in-callback.t`, 8 assertions (including the subclass
refusal), passing unchanged under rakudo.
