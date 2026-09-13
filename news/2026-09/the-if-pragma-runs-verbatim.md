# The `if` pragma runs verbatim: `Raku.legacy`, and a slang's actions role

`use Foo:if($cond)` — conditional module loading, the Raku port of Perl's `if`
pragma — was unreachable on mutsu. Eighteen distributions in the `ecosystem/`
parity corpus depend on the [`if`](https://raku.land/zef:lizmat/if)
distribution, fifteen of them `blocked_load`, and all of them died on the same
line:

```
slang activation for 'if' failed: No such method 'legacy' for invocant of type 'Raku'
```

`Cro::HTTP` and `Cro::WebSocket` are in that list (through `Crypt::Random`),
so the real reach was wider than eighteen.

## `Raku.legacy` is a question about *which mutsu* the module is talking to

`Raku.legacy` is rakudo's one `Raku:U` method: it tells a module which compiler
frontend is compiling it — `True` under the legacy NQP/`Perl6::World`
frontend, `False` under RakuAST. The `if` pragma's `sub EXPORT` branches on it,
and both branches are compiler guts: `True` mixes a role into the `$*W` World
to override `do_pragma_or_load_module`; `False` registers a slang through
`$*LANG.define_slang` whose actions role overrides `statement-control:sym<use>`.

So implementing the method only moves the failure. The decision
([ADR-0098](../../docs/adr/0098-if-pragma-actions-slang.md)) is that mutsu
answers **`False`**: the compile-time surface mutsu offers a module is the
RakuAST-shaped one — a `$*LANG` handle with `define_slang` / `slang_grammar` /
`slang_actions`, from ADR-0026 and ADR-0091 — and there is no `$*W` World to
mix a role into, nor any prospect of one (both of those ADRs already declined
to build that surface). Answering `True` and then not honouring the branch
would be a lie the module acts on.

## ADR-0026 read half of a `define_slang` call; now it reads both

ADR-0026 mapped the grammar rules a slang's roles **override** onto parser
modes, and left the *actions* handle recorded-but-inert because
`Slang::Tuxic` — the only slang in the corpus at the time — passes `Mu` for
actions. Its own §5 asked whether that would have to change. The `if` pragma
answers yes: its entire registration is one actions-role method.

The two halves of a registration spell an override differently, and
`define_slang` is now told which it is looking at:

| handle | an override is |
| --- | --- |
| `slang_grammar('MAIN')` | a `token`/`rule` member (plus an `L10N::XX` role's `<category>2ast` spelling map) |
| `slang_actions('MAIN')` | a **method**, one per production the role overrides |

An actions class in Rakudo has exactly one method per grammar production, so
the method *name* is the override name by construction — no heuristic, and
nothing in the body is read. ADR-0026 §4's refusal to execute Rakudo-internal
bodies survives intact: `if`'s method calls
`RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE`, `$/.panic` and `nextsame`, and
mutsu runs none of it. An override name the recognized map does not know is
still a hard error naming it.

The map gains one entry: `statement-control:sym<use>` sets a unit-scoped
`use_if_adverb` mode, exactly as `term:sym<identifier>` sets Tuxic's
`spaced_call`.

## The native provider is gone

mutsu already understood `:if(EXPR)` — natively. `use if;` compiled to nothing
(a name-keyed no-op arm in the compiler) and the adverb was honoured whether or
not the pragma was in scope. That is a rung-3 native provider of an ecosystem
distribution, banned by ADR-0096, and it diverged from rakudo in both
directions: rakudo treats an adverb no pragma claimed as inert and loads the
module anyway.

`if` 0.1.5 (`zef:raku-community-modules`, Artistic-2.0, zero dependencies, 93
lines) is now bundled at `modules/if/` and resolves with zero config; the no-op
arm is deleted; and `:if` is honoured only while the pragma's mode is on. The
mode keys on what the module *declares*, not on it being called `if`, so the
bundled copy is load-bearing rather than decoration.

```
$ mutsu -e 'use Totally::Nonexistent:if(False); say "ok"'
Could not find Totally::Nonexistent in:
    (module repositories)
$ mutsu -e 'use if; use Totally::Nonexistent:if(False); say "ok"'
ok
```

Both answers match rakudo.

## A module's `my role` now survives an `EVAL` that loaded it

Getting the pragma's own upstream suite to run surfaced a general bug. mutsu
tore down every `my role` declared while it was inside an `EVAL` — module loads
included. But a module's compunit outlives the EVAL that happened to trigger
the load, and re-`use`ing it re-runs only its `sub EXPORT`, never its mainline.
For `if`, whose EXPORT re-reads the role its mainline declared, the second
`EVAL 'use if'` in a process died with `Slang activation: 'Actions' is not a
known role`. A role declared by the EVAL *string* is still EVAL-local; one
declared by a module it loaded is not. Pinned by
`t/modules/module-loaded-in-eval-keeps-my-roles.t`, green under rakudo too.

## Verification

`t/modules/import-export/use-if-pragma.t` passes 9/9 on mutsu — and 9/9 under
`RAKUDO_RAKUAST=1`, the frontend the decision claims, with the one difference
under rakudo's legacy frontend being the deliberately mutsu-specific
`Raku.legacy` assertion itself.

What is deliberately not closed: rakudo BEGIN-evaluates the `:if` value and
drops a false `use` at compile time, while mutsu still evaluates it at run
time. Every consumer shape in the corpus is a platform or compiler-version test
(`$*DISTRO.is-win`, `$*PERL.compiler.version < v2018.12`) that answers the same
either way; a condition only known at run time is the visible difference.
`if`'s own suite is not whitelisted in the batteries gate yet either — its one
file counts loads through a caller dynamic variable, which a module load does
not write back on mutsu ([#8229](https://github.com/tokuhirom/mutsu/issues/8229)),
a gap unrelated to the pragma.

Closes [#8210](https://github.com/tokuhirom/mutsu/issues/8210).
