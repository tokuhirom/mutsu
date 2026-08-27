# A user lexical `$self` no longer collides with a method's invocant

`self` is a *term* in Raku, not a `$`-sigiled variable, so `my $self` is an ordinary
lexical that never interacts with a method's invocant. mutsu stores scalars **sigil-less**,
which put `my $self` on the env key `"self"` — exactly the key the invocant binds under.
The two were one storage location, and whichever was written last won.

That was a Tier S soundness bug with two faces. Silent corruption:

```raku
class Outer { method tag { 'OUTER' } }
class Inner { method tag { 'INNER' } }

my $self = Outer.new;
my $m = method () { $self.tag };
say $m(Inner.new);          # raku: OUTER    mutsu: INNER
```

and a hard stack overflow, in the shape that actually shows up in real distributions:

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

The `FETCH` method literal bound *its* invocant — the `Proxy` itself — to `"self"`,
replacing the captured lexical; `$self.nodes` then dispatched against the Proxy, whose
every method access deref'd through `FETCH` again, forever. Renaming the lexical
(`my $outer = self`) made both cases correct, which was the whole tell.

## The fix

[ADR-0061](../../docs/adr/0061-lexical-self-has-its-own-env-key.md) records the decision:
**the env key `"self"` names the invocant and nothing else; a user lexical spelled
`$self` gets the reserved key `"$self"`.** No ordinary scalar carries its sigil in mutsu's
env, so that key cannot collide — and because what follows the sigil is a plain lowercase
identifier, it flows through the ordinary lexical machinery (free-variable capture,
closure upvalues, block scoping, cross-thread sharing) with no special cases.

The rename lives where a name becomes a key. The parser emits `Expr::Var("$self")` for a
read, and a new `lexical_var_name()` wrapper applies the same rename at the declaration
and assignment sites (`my $self`, `$self = ...`, `constant $self`, `my ($self, $x)`),
which take the name from the raw token rather than from an `Expr::Var`. That also removes
an ambiguity the AST used to lose: `Stmt::Assign { name: "self" }` was produced by *both*
`self = 5` and `$self = 5`, so the "cannot assign to the invocant" check had to be gated
on `lexically_in_method` to avoid rejecting a legitimate `my $self`. Now only the bare
term produces that shape.

A *signature parameter* spelled `$self` keeps its sigil-less `ParamDef.name` — signature
introspection re-adds the sigil, and a named parameter matches its argument `Pair` by that
name — so it still binds the plain key. The compiler records
`self_is_signature_param` for a routine that declares one and maps `"$self"` back onto
`"self"` inside it, inheriting the flag into nested blocks and closures exactly as the
lexical scope does. A parser-synthesized anonymous invocant (`method () { ... }`,
`method (Foo:D:)`, `method (::?CLASS:)`) is recorded under the name `self` too but declares
no lexical, so those carry an `IMPLICIT_INVOCANT_TRAIT` marker and
`ParamDef::declares_self_lexical()` is the one oracle that tells the two apart.

The ADR records why the two obvious alternatives lost: re-keying the *invocant* instead
would have touched roughly 120 env-key sites across `src/runtime/` and `src/vm/` for no
extra correctness, and a runtime "read `$self`, fall back to `self`" would have been
silently wrong whenever a routine had both an outer `my $self` and its own `$self`
parameter — the compile-time flag cannot mis-bind that way.

## Results

- `t/lexical-self-vs-invocant.t` pins 18 assertions covering every direction, including
  the negative ones: bare `self` in a method is still the invocant; `method bar($self: $n)`
  still binds `$self` to the invocant; `method m(C:D:)`'s anonymous invocant marker does
  *not* shadow an outer `$self`; `sub ($self)` / `-> $self, $x` parameters keep working,
  including from a nested closure; and `self = 5` inside a method still dies while
  `my $self = 1; $self = 2` inside one now works (it used to be rejected as an assignment
  to the invocant).
- **`XML` battery: 5/15 → 9/15** upstream test files (`raku`: 15/15). All four files that
  flipped were aborting on `$doc.root[0]`, since `XML::Element` implements both `AT-POS`
  and `AT-KEY` with the `my $self = self` + `Proxy` idiom. `docs/batteries/xml.md` is
  updated with the new count and the now-dominant blockers.

Two neighbouring defects found while verifying the fix are filed separately, since both
reproduce with an ordinary variable name and are untouched by this change:
`todo/tickets/proxy-at-pos-store-and-shadowed-capture.md` (a `Proxy` returned from an
`is rw` `AT-POS` loses its STORE, and its deferred capture loses to a same-named outer
lexical) and `todo/tickets/proxy-what-reports-proxy-instead-of-fetching.md` (`.WHAT` on a
`Proxy` answers about the container instead of FETCHing first).
