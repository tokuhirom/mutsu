# A user lexical named `$self` is clobbered by the invocant inside any `method` block

## Symptom

`my $self = ...` is an ordinary lexical scalar in Raku — `self` is a *term*, not a
`$`-sigil variable, so the two never interact. In mutsu they share one env key, so a
`$self` captured by a closure is silently replaced by whatever invocant the closure is
later called with:

```raku
class Outer { method tag { 'OUTER' } }
class Inner { method tag { 'INNER' } }

my $self = Outer.new;
my $m = method () { $self.tag };
say $m(Inner.new);          # raku: OUTER    mutsu: INNER

class C {
    method make-cb { my $self = self; method () { $self.^name } }
    method tag { 'C' }
}
say C.new.make-cb()(Inner.new);   # raku: C    mutsu: Inner
```

The damaging case is a `Proxy`, where the substituted invocant is the Proxy itself and
reading it re-enters `FETCH` forever:

```raku
class B {
    has @.nodes;
    method AT-POS($offset) is rw {
        my $self = self;
        Proxy.new(
            FETCH => method () { $self.nodes[$offset] },
            STORE => method ($val) { $self.nodes[$offset] = $val }
        )
    }
}
say B.new(nodes => ['x','y'])[1];
# raku:  y
# mutsu: thread 'mutsu-main' has overflowed its stack / fatal runtime error
```

Renaming the lexical (`my $outer = self`) makes both work, which is the whole tell.

## Root cause

mutsu stores scalars **sigil-less**: `my $self` becomes the env key `self`, which is
exactly the key a method's invocant is bound under. `src/compiler/stmt.rs` already
documents the collision from the other direction — it has to gate its "`self` is
immutable, reject assignment" check on `lexically_in_method` precisely because
"scalars are stored sigil-less and would otherwise collide" with a plain `my $self`.

The AST *does* distinguish the two: `$self` parses to `Expr::Var("self")` and the bare
term to `Expr::BareWord("self")`. Only the storage key is shared. So when the FETCH
`method` binds its own invocant into the call frame as `self`, it shadows the captured
lexical, and `$self.nodes` dispatches against the Proxy — whose every method access
derefs through `FETCH` again.

## Why this is `deep/`, not a ticket

The fix is to stop sharing the key, and the key is load-bearing in ~190 places
(`git grep '"self"' src | wc -l`). Two shapes were considered:

1. **Give the invocant its own key** (`__MUTSU_SELF__`) and compile the `self` term to
   read it. Cleanest semantically, but touches every one of those sites — parser
   (implicit and explicit `$self:` invocant params), compiler, VM var ops, proto
   dispatch's save/restore of `env["self"]`, the map/grep `touched_keys` machinery,
   thread env cloning, `did_you_mean`, …
2. **Give the user scalar its own key** (`"$self"`, with the sigil, which no other
   scalar uses) and have `Expr::Var("self")` read `"$self"` with a fallback to `"self"`.
   Narrower, and the fallback preserves `method bar($self: $n) { $self }` (an explicit
   invocant param is *named* `self` and binds the plain key). But it still has to be
   threaded consistently through declaration, assignment, `local_map` slot allocation,
   and closure free-variable capture, any of which silently mis-binding would be worse
   than the current loud stack overflow.

Either way this needs a decision recorded before it is coded, not an in-flight guess.

## Blast radius / who is waiting on it

- **The `XML` battery.** `XML::Element` implements both `AT-POS` and `AT-KEY` with the
  `my $self = self;` + `Proxy` idiom above, so `$doc.root[0]` — the most ordinary thing
  you can do with a parsed document — aborts the process. It accounts for 7 of the 10
  files still failing in `XML`'s upstream suite (5/15 as of 2026-08-26); see
  `docs/batteries/xml.md` and `todo/tickets/bundle-xml-battery.md`.
- The `my $self = self` + `Proxy` pattern is the standard Raku way to write an `is rw`
  `AT-POS`/`AT-KEY`, so any dist with a custom container hits this.

## Related, smaller divergence found alongside

`.WHAT` on a `Proxy` reports `(Proxy)` under mutsu and `(Str)` under raku — i.e. mutsu
does not FETCH before answering. Independent of the collision above (it reproduces with
a Proxy that closes over nothing), and much smaller; worth its own ticket if someone
picks it up separately.

## Repro

```sh
cargo build
timeout 10 ./target/debug/mutsu -e 'my $self = 1; class C { method m { say $self } }; C.new.m'
```

## Discovered via

Re-measuring the three "bundle X once its blockers clear" battery trackers on
2026-08-26 (`docs/batteries/xml.md`): after the grammar-dynvar, indirect-type-param and
group-backreference fixes, this became `XML`'s dominant remaining blocker.
