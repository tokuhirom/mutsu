# ADR-0098: mutsu answers `Raku.legacy` with `False`, and a slang's *actions*-role method is an override name

- Status: Accepted, implemented (2026-09-13)
- Date: 2026-09-13
- Deciders: tokuhirom, Claude
- Extends: [ADR-0026](0026-slang-activation-architecture.md) (slang activation
  — the `$*LANG` handle, the activation sub-interpreter, the
  recognized-override map) and [ADR-0091](0091-slang-package-declarators.md)
  (a slang may also *add* a package declarator). Both stay in force; this ADR
  adds a third thing a slang may register and keeps ADR-0026 §4's refusal to
  execute Rakudo-internal token bodies.
- Related: [BATTERIES.md](../../BATTERIES.md) §1 and
  [ADR-0096](0096-batteries-adoption-policy.md) (rung 2: grow the interpreter
  until the real, vendored module runs verbatim; native provision banned
  2026-08-01), [#8210](https://github.com/tokuhirom/mutsu/issues/8210)

## 1. Context

The zef distribution [`if`](https://raku.land/zef:lizmat/if) implements the
`:if(…)` adverb on a `use` statement — conditional module loading, the Raku
port of Perl's `if` pragma. Eighteen distributions in the `ecosystem/` parity
corpus depend on it, fifteen of them `blocked_load`, and every one of them
died the same way:

```
slang activation for 'if' failed: No such method 'legacy' for invocant of type 'Raku'
```

`Raku.legacy` is the first thing the module's `sub EXPORT` asks, and the two
branches it selects between are both compiler guts:

```raku
sub EXPORT(|) {
    if Raku.legacy {
        $*W.HOW.mixin($*W, BetterWorld);          # override do_pragma_or_load_module
    }
    else {
        my $LANG := $*LANG;
        $LANG.define_slang('MAIN',                 # override statement-control:sym<use>
          $LANG.slang_grammar('MAIN'),
          $LANG.slang_actions('MAIN').^mixin(Actions)
        );
    }
    BEGIN Map.new
}
```

So implementing `Raku.legacy` only moves the failure: whichever branch mutsu
answers for, it then has to support. That is the decision this ADR records.

### 1.1 What mutsu already had, and what was wrong with it

mutsu understood `use Foo:if(EXPR)` natively: `use_decl.rs` parsed the adverb
into `Stmt::Use::condition`, the compiler guarded the `UseModule` op with it,
and `use if;` compiled to nothing at all (a name-keyed no-op arm in
`compiler/stmt.rs`). That is a rung-3 native provider of an ecosystem
distribution — banned by ADR-0096 — and it diverged from Rakudo in both
directions: `use Foo:if(False)` skipped the load on mutsu with no pragma in
sight, while on Rakudo an unrecognized `use` adverb is inert and the module
loads.

## 2. Decision

### 2.1 `Raku.legacy` is `False`

`Raku.legacy` tells a module which compiler frontend is compiling it: Rakudo
answers `True` under the legacy NQP/`Perl6::World` frontend and `False` under
RakuAST (measured on rakudo 2026.07, with and without `RAKUDO_RAKUAST=1`).

mutsu is neither, so the question is which branch it can *honour*. The
compile-time surface mutsu offers a module is the RakuAST-shaped one — a
`$*LANG` handle with `define_slang` / `slang_grammar` / `slang_actions`
(ADR-0026, ADR-0091) — and there is no `$*W` World object to mix a role into,
nor any prospect of one (ADR-0026 §4 and ADR-0091 §4 both rejected growing
that surface). `False` is therefore the honest answer, and it is the answer
mutsu gives to any module asking.

It is a `Raku:U`-only method in Rakudo: `Raku.backend` is "No such method" and
`$*RAKU.legacy` is an invocant-type error. mutsu matches the shape (both still
fail), so `legacy` is a type-object arm of its own rather than a row in
`Raku`'s native-method table. The one divergence left is the *message* of the
`$*RAKU.legacy` failure, since mutsu does not model `:U`-only invocants.

### 2.2 An actions role's methods are override names

ADR-0026 §2.2 read the roles mixed into the **grammar** handle and mapped the
names of the `token`/`rule` members they override onto parser modes. It left
the **actions** handle recorded-but-inert, because `Slang::Tuxic` — the only
slang in the corpus at the time — passes `Mu` for actions. Its §5 asked
whether that would have to change; the `if` pragma answers yes: its entire
registration is one actions-role method.

`define_slang` now reads both halves. The two spell an override differently
and the reader is told which it is looking at:

| handle | what an override looks like |
| --- | --- |
| `slang_grammar('MAIN')` | a `token`/`rule` member (plus, for an `L10N::XX` role, a `<category>2ast` spelling map) |
| `slang_actions('MAIN')` | a **method**, one per production the role overrides |

An actions class in Rakudo has exactly one method per grammar production, so
the method *name* is the override name by construction — no heuristic is
needed, and nothing in the body is read. ADR-0026 §4's refusal to execute
Rakudo-internal bodies is preserved verbatim: `if`'s method body calls
`RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE`, `$/.panic` and `nextsame`, and
mutsu runs none of it.

An override name that is not in the recognized map is still a hard error
naming it (ADR-0026 §2.2), which now also covers an actions role carrying a
method mutsu does not recognize as a production.

One name is read from the grammar half **only**: a
`package_declarator:sym<...>`. ADR-0091 registers a declarator from that
candidate, and Rakudo pairs every grammar candidate with an actions method of
the same name whose job is to build QAST — the half mutsu never runs. Reading
the actions one as a second registration would overwrite the grammar one with a
bodyless record and lose its `$*PKGDECL` / `set_how`, turning a `role`
declarator back into a class (caught by
`t/modules/slang-package-declarator.t`). The actions counterpart is skipped, not
errored: it is the expected other half of a registration mutsu already has.

### 2.3 `statement-control:sym<use>` maps onto a `:if` adverb mode

The recognized-override map gains one entry: `statement-control:sym<use>` sets
a unit-scoped `SlangModes::use_if_adverb`, exactly as
`term:sym<identifier>` sets `spaced_call`. While it is on, `:if(EXPR)` on a
`use` is a load condition; while it is off, the adverb is consumed and
discarded like a `:auth(...)` selector and the module loads unconditionally —
which is what stock Rakudo does with an adverb no pragma claimed.

That makes the bundled module load-bearing, the property ADR-0026 §4 chose
over name-keyed hardcoding: the mode keys on what the module *declares*, not
on the module being called `if`.

### 2.4 The real distribution is bundled; the native provider is deleted

`if` 0.1.5 (`zef:raku-community-modules`, Artistic-2.0, zero dependencies, one
93-line file) is vendored to `modules/if/` and resolves with zero config, and
the `use if;` no-op arm in `compiler/stmt.rs` is gone. `use if;` now loads the
real module, whose `sub EXPORT` runs for real and registers the slang. Record:
[docs/batteries/if-pragma.md](../batteries/if-pragma.md).

## 3. Consequences

- The `if` pragma's `EXPORT` runs verbatim on mutsu, so the fifteen
  `blocked_load` distributions that only ever failed on it get past that wall.
  `Crypt::Random` — a bundled battery and a Cro dependency, and the module
  whose load error most of them actually reported — is unaffected in behaviour
  and now takes the real path.
- The upstream suite is oracle-checked in a stronger sense than usual:
  `t/modules/import-export/use-if-pragma.t` passes 9/9 under
  `RAKUDO_RAKUAST=1` — the frontend §2.1 chose — and 8/9 under the legacy
  frontend, the one difference being the deliberately mutsu-specific
  `Raku.legacy` assertion.
- **A `my role` declared by a module loaded from inside an `EVAL` now survives
  the EVAL.** mutsu tore down every `my role` declared while it was inside an
  EVAL, module loads included; a re-`use` of such a module re-runs only its
  `sub EXPORT`, which for `if` re-reads the role its mainline declared, so the
  second `EVAL 'use if'` in a process died with "Slang activation: 'Actions'
  is not a known role". The module's compunit outlives the EVAL that triggered
  the load, so the role does too. Pinned by
  `t/modules/module-loaded-in-eval-keeps-my-roles.t` (green under rakudo as
  well).
- **What is deliberately not supported.** Rakudo BEGIN-evaluates the `:if`
  value (`RakuAST::BeginTime.IMPL-BEGIN-TIME-EVALUATE`) and replaces a false
  `use` with `RakuAST::Statement::Empty` at compile time. mutsu keeps its
  existing runtime evaluation of the condition: the parse still registers the
  named module's exports and still runs its slang scan, and only the load is
  skipped. Every consumer shape in the corpus is a platform or
  compiler-version test (`$*DISTRO.is-win`, `$*PERL.compiler.version < v2018.12`)
  that answers the same either way. The visible difference is a condition
  whose value is only known at run time — `my $c = True; use Foo:if($c)` loads
  `Foo` on mutsu and not on Rakudo, where `$c` is undefined at BEGIN.
- The mode is unit-scoped, like every other slang mode: an `EVAL` string is its
  own compilation unit and does not inherit the pragma. The test file relies on
  exactly that to exercise both sides.

## 4. Alternatives considered (rejected)

- **Answer `Raku.legacy` with `True`.** It is the branch rakudo's *default*
  frontend takes, so it looks like the compatible answer, but it asks mutsu for
  a `$*W` World object with a `do_pragma_or_load_module` method to override,
  and for `.HOW.mixin` on it — the compiler-guts surface ADR-0026 §4 and
  ADR-0091 §4 both rejected building. Answering `True` and then not honouring
  the branch is worse than answering `False`: it is a lie the module acts on.
- **Implement `Raku.legacy` and stop there**, leaving the native `:if`
  provider in place. The eighteen distributions would load, but mutsu would
  keep a private dialect (`:if` honoured with no pragma) and the real module
  would be dead weight — precisely what BATTERIES.md §1 and ADR-0026 §4 forbid.
- **Read the actions role's method bodies** rather than only their names. The
  body is Rakudo's RakuAST-actions surface (`$*R`, `$*CU.context`,
  `RakuAST::BeginTime`, `nextsame` into a real actions class); running it is
  the different project ADR-0026 §4 already declined, and nothing in the corpus
  needs the *behaviour* of a body, only the registration.
- **Key the mode on the module name `if`.** Name-keyed native provision in
  disguise, rejected for `Slang::Tuxic` (ADR-0026 §4) and `Test::Async`
  (ADR-0091 §4) on the same grounds.

## 5. Open questions

- BEGIN-time evaluation of the `:if` value (§3) is the remaining divergence.
  Closing it needs a general compile-time-`use` evaluation slice, which is the
  same ADR-0026 §2.1 gap that a genuine compile-time `use` has always needed.
- A grammar role that happens to carry a plain helper method is still read the
  old way (methods ignored unless they are a vocabulary map); an *actions* role
  with a helper method would fail loudly. No corpus module does this, and
  failing loudly is ADR-0026's chosen behaviour for anything unrecognized.
