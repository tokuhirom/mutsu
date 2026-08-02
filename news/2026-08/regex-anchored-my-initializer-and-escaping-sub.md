# An anchored `:my` initializer dispatches, and a block-lexical sub can escape

Two interpreter fixes, both surfaced by Cro's route dispatcher. Together they
close the "anchored array `:my` loses its elements" blocker recorded in
`todo/deep/cro-http-request-hang-short-name-env-pollution.md`: Cro's generated
path matcher now produces the right `Capture` for a route with path segments.

## A `:my` initializer behind an anchor runs with full dispatch

A regex declarator reaches one of two paths. `parse_regex_declarative_prefix`
hoists `:my` out of the pattern when it sits at the very front; a **leading
anchor makes that scan stop immediately**, so the declaration instead becomes a
`RegexAtom::VarDecl` atom evaluated at match time. That atom ran the whole
declaration in a scratch `Interpreter` and harvested its env diff — and a
scratch interpreter carries only a lean registry copy with **no classes**, so an
initializer that calls a method simply failed:

```raku
class Req { method segs() { <a b c> } }
my $*R = Req.new;
say ("x" ~~ / ^ :my @s = $*R.segs; 'x' { make @s.elems } /).ast;   # was 0, now 3
say ("x" ~~ / ^ :my $r = $*R; :my @s = $r.segs; 'x' { make @s.elems } /).ast;
                                                                  # was 1, now 3
```

Each non-dynamic declaration's initializer is now evaluated on the **real**
interpreter, with the lexicals declared earlier in the same pattern installed
around the call and restored afterwards — so a later declaration can read, and
dispatch on, an earlier one (`:my $req = …; :my @segs = $req.path-segments;`,
which is Cro's shape exactly). Dynamics (`:my %*PLAYED = ()`) and every other
statement form keep the scratch path and its env-diff harvest, which now runs
only when something is left for it to do.

## A block-lexical `sub` stays callable from a closure that escapes the block

`Cro::HTTP::RequestParser` declares `my sub fresh-message` inside a `supply`
block and calls it from a `whenever` closure. Block exit restores the routine
registry, so the name stopped resolving and every connection died with "Unknown
function: fresh-message".

A non-multi, non-exported sub declared at block scope now also stores its `Sub`
value in env under a reserved key (`__mutsu_block_lexical_sub::`), which the
closure's capture carries out and a new last-resort branch in the call fallback
dispatches. Three gates matter, and each was learned from a regression:

- The key is deliberately **not** the plain `&name`. While the block is live the
  registry entry is authoritative — it is what carries `state` variables — and
  the bareword and call paths consult `&name` *ahead* of it, which reset the
  state on every call (`sub f { ++state $ }` returned `1 1 1`).
- An **EVAL'd** compilation unit also runs at raised block depth, but a sub it
  declares is lexical to that unit and must stay invisible afterwards
  (`EVAL q|sub zzz9 {…}|; zzz9()` dies in raku).
- An **exported** sub is part of its module's interface, installed by the export
  machinery. Registering it here also put the reserved key into the module-load
  env diff, which broke `require M <quux>`'s missing-symbol detection
  (`roast/S11-modules/require.t`).

Pinned by `t/regex-my-initializer-and-escaping-sub.t`, plus
`t/cross-module-short-name-types.t`, which adds a cross-*compunit* regression
guard for the short-name collision that started the Cro investigation — the
owner-scope fix's pin is in-file only, and module loading order is what polluted
the one global env.

## What still blocks a parameterised Cro route

The matcher now returns the right `Capture`, but the route's bind check
(`<?{ my $han = @handlers[$i]; $han.signature.ACCEPTS($cap) … }>`) reads an
empty `@handlers`: the matcher is an `EVAL`'d regex, and a regex value is a bare
pattern string, so embedded code resolves its free variables against the
*match-time* env rather than the defining scope. Recorded, with a reverted
prototype's design and two candidate representation fixes; fixed shortly
afterwards — see `news/2026-08/regex-literals-are-closures.md`.
