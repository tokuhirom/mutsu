# A `whenever` body is a closure compiled where it is written

A `whenever` callback used to be rebuilt from the body's AST every time the
statement ran, and the carrier recompiled that AST on every dispatch. The
recompiled chunk bound the body's parameter but never vouched for it, so a
closure created in one invocation read `$x` from whichever invocation of the
same body later called it:

```raku
my $s = Supplier.new;
my @cl;
my $outer = supply {
    whenever $s.Supply -> $x {
        if $x eq 'stop' { .() for @cl }
        else { @cl.push: { say "closure sees $x" } }
    }
}
$outer.tap;
$s.emit('a'); $s.emit('stop');   # was "closure sees stop", now "closure sees a"
```

The compiler now compiles the body, and each of its `LAST` and `QUIT` phasers,
as an ordinary closure at the `whenever` site (`OpCode::WheneverScope` carries
their code indices), and the VM builds them with the shared closure pipeline.
Their free variables are captured like any closure's, and the drive loop and
supply taps dispatch them as bytecode (#12872, ADR-12529).

That retires the machinery that only existed because the body was AST:

- the runtime phaser split and its per-site cache (`whenever_body_splits`);
- the per-dispatch compile of `whenever` callbacks in `call_react_callback`;
- the owned-lexical hand-down to nested `whenever`s through the carrier
  (`CompiledCode::inherited_owned_lexicals`,
  `pending_whenever_inherited_owned`) — a nested `whenever` now inherits the
  enclosing callback's vouched captures through `frame_authoritative`;
- the whole-env capture and env-sync slots a frame containing a `whenever`
  was forced into, because its body was invisible to the free-variable scan.

A `whenever` body's `$_` also follows the block's signature on every dispatch
path: with a declared signature it stays the enclosing topic.
