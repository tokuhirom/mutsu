# ADR-13220: An actions-role method is a production handler only if the grammar has a production of that name; every other method is an inert helper

- **Status**: Proposed (2026-10-21). On acceptance it supersedes
  [ADR-0098](0098-if-pragma-actions-slang.md) §2.2's rule that *every* plain
  method of an actions role is an override name, and answers ADR-0098 §5's
  second open question. The rest of ADR-0098, and
  [ADR-0026](0026-slang-activation-architecture.md) §4 /
  [ADR-0091](0091-slang-package-declarators.md) §4 (no action or token body is
  ever executed, no `$*W`, no QAST), stay in force unchanged.
- **Date**: 2026-10-21
- **Deciders**: tokuhirom, Claude
- **Issue**: [#13220](https://github.com/tokuhirom/mutsu/issues/13220)
- **Related**: [#9926](https://github.com/tokuhirom/mutsu/issues/9926)
  (role-kind slang declarator HOW not attached — slice 2 below),
  [#14290](https://github.com/tokuhirom/mutsu/issues/14290) (a method's
  `nqp::list` result is not HLL-ized — the wall after this one),
  [ADR-0096](0096-batteries-adoption-policy.md) (rung 2: the real module runs
  verbatim; no native provision)

## 1. Context

`use Test::Async::Decl` (Test::Async 0.1.17) fails on mutsu:

```
slang activation for 'Test::Async::Decl' failed: Slang activation NYI: grammar
rule override 'add_phaser' is not supported by this implementation (...)
```

The module's `sub EXPORT` calls `$*LANG.define_slang('MAIN', <grammar>,
$*LANG.actions.^mixin(TestAsyncActions))`, and the actions role is:

```raku
my role TestAsyncActions {
    sub mkey ( Mu $/, Str:D $key ) { ... }
    method add_phaser(Mu $/) {                      # helper
        my $blk := QAST::Block.new( ... HubHOW.register-bundle($*TEST-BUNDLE-TYPE) ... );
        $*W.add_phaser($/, 'ENTER', $*W.create_code_obj_and_add_child($blk, 'Block'));
    }
    method package_declarator:sym<test-hub>(Mu $/)      { $/.make( mkey($/, 'package_def').ast ) }
    method package_declarator:sym<test-bundle>(Mu $/)   { self.add_phaser($/); $/.make(...) }
    method package_declarator:sym<test-reporter>(Mu $/) { self.add_phaser($/); $/.make(...) }
}
```

ADR-0098 §2.2 decided that "an actions class in Rakudo has exactly one method
per grammar production, so the method *name* is the override name by
construction". `slang_role_rule_names` (`src/runtime/slang_activation.rs`)
implements exactly that, so `add_phaser` is read as an override of a grammar
rule called `add_phaser`, which the recognized-override map does not know, and
activation is a hard error. ADR-0098 §5 had foreseen the case ("an *actions*
role with a helper method would fail loudly") and deferred it because no corpus
module did it. Two corpus distributions now do: Test::Async itself, and
Config::BINDish 0.0.18, whose `Config::BINDish::Test` loads Test::Async::Decl.

This is also a regression against ADR-0091, which made Test::Async's three
declarators work (2026-09-12); ADR-0098 (2026-09-13) started reading the
actions half and so started rejecting the helper that sits next to their
actions.

### 1.1 The premise of ADR-0098 §2.2 is measurably false

Rakudo 2026.09, introspecting `$*LANG` and `$*LANG.actions` at `BEGIN` time:

| frontend | grammar regexes (productions) | actions methods | actions methods naming no production |
| --- | --- | --- | --- |
| legacy (`RAKUDO_RAKUAST=0`) | 692 | 336 | 29 |
| RakuAST (default) | 861 | 450 | 81 |

Rakudo's own actions classes carry helper methods. Nothing about an actions
class makes "method" mean "production handler".

### 1.2 What Rakudo actually does with an actions method

The parser looks an action up by **the name of the production that just
matched** (`!reduce` → `findmethod($actions, $name)`). An actions method is
therefore a production handler exactly when the (slang-extended) grammar has a
production of that name; any other method is never called by the parse and is
reachable only from other action bodies, as `self.add_phaser($/)` is. That is
an exact, static rule, and it needs nothing from a method body.

### 1.3 What the helper's body is for — and why mutsu does not need it

The second gap the issue names is the body itself: `add_phaser` builds QAST and
calls `$*W.add_phaser` to attach an `ENTER` phaser that runs
`Test::Async::Metamodel::HubHOW.register-bundle($*TEST-BUNDLE-TYPE)` when a
`test-bundle` / `test-reporter` package body is entered. ADR-0026 §4 and
ADR-0091 §4 rejected executing that surface, and this ADR does not revisit it.

It turns out not to be needed. The bundle's metaclass already registers it:

```raku
# lib/Test/Async/Metamodel/BundleHOW.rakumod
method new_type(|) {
    $*TEST-BUNDLE-TYPE := callsame;
    unless $*W && $*W.is_precompilation_mode {
        Test::Async::Metamodel::HubHOW.register-bundle: $*TEST-BUNDLE-TYPE;
    }
    $*TEST-BUNDLE-TYPE
}
```

`new_type` registers the bundle directly unless the module is being
precompiled; the `ENTER` phaser exists for the precompiled case, where
`new_type` ran in a different process and the registration (a module-level
lexical of `HubHOW`) was lost. `register-bundle` dedups by name, so in a
non-precompiled Rakudo run the phaser is a no-op repeat. mutsu has no
precompilation and binds no `$*W` (ADR-0098 §2.1), so it is permanently on the
non-precompiled path: what it needs is for the `test-bundle` declaration to
**call `BundleHOW.new_type`** — which a role-kind slang declarator does not do
today (ADR-0091 §3, "recorded but not attached"; #9926 item 1).

### 1.4 There is an oracle

The issue records Test::Async as `no_baseline` because Rakudo fails on
`unit test-hub Test::Async::Hub;`. That is the RakuAST frontend, now Rakudo's
default, which does not honour the legacy `define_slang` registration. Under
`RAKUDO_RAKUAST=0` the distribution works: 27 of its 28 test files pass
(`t/005-bootstrap.rakutest` fails tests 11–12). Test::Async::Decl is written
for the legacy frontend only (it never asks `Raku.legacy`), so the legacy run
is the meaningful reference for this module.

## 2. Decision

### 2.1 Classify each actions-role method by the grammar, not by being a method

In `define_slang`, a plain method `m` of an actions role is a **production
handler** iff at least one of:

1. `m` is a token/rule declared by a grammar role mixed into the *same*
   `define_slang` call, or by a slang already active in this compilation unit
   (`defined_slang_rules` / `defined_slang_declarators`);
2. `m` is a production of Rakudo's host grammar (§2.2);
3. `m` has the shape `<proto>:sym<...>` and `<proto>:sym<...>` candidates of
   that `<proto>` exist in the host grammar (§2.2) — a candidate of a known
   proto family is treated as a production even if neither grammar declares
   that exact candidate.

Every other plain method is an **inert helper**: it is not an override, it is
not an error, and — like every action body — it is never run. A `sub` inside
the role (`mkey`) is already not a method and is unaffected.

A production handler is treated exactly as ADR-0098 §2.2 treats every method
today: recognized names map onto parser modes, the
`package_declarator:sym<...>` actions counterpart of a grammar candidate is
skipped (ADR-0098 §2.2's last paragraph), the `<category>2ast` / `<category>2str`
vocabulary maps are read as before, and **any other production handler is
still a hard error naming it** (ADR-0026 §2.2).

Rule 3 is deliberately stricter than Rakudo, which would silently never call a
`statement_control:sym<frobnicate>` action that no grammar declares. It is the
guard against a stale table (§2.2): a slang targeting a candidate added in a
Rakudo release newer than the table fails loudly instead of being classified
as a helper and silently ignored. A plain-identifier production newer than the
table is the one residual way a real override could be misread as a helper;
regenerating the table on oracle upgrades (§2.2) closes it.

For Test::Async::Decl: the three `package_declarator:sym<test-*>` methods are
handlers by rule 1 (and rule 3) and are skipped as the actions halves of
ADR-0091 registrations; `add_phaser` matches none of the rules and is a helper.
For `if`: `statement-control:sym<use>` is a handler by rule 2 and keeps
mapping onto `use_if_adverb`. The L10N roles have no actions half at all (they
are mixed into `slang_grammar('MAIN')` only). No corpus slang changes
classification except Test::Async::Decl.

### 2.2 The host-grammar production table is generated data, checked in

mutsu's parser is not a grammar with named productions, so the set in rules 2
and 3 is a **checked-in data file** of production names: the union of the
regex methods of `$*LANG` under both Rakudo frontends (legacy names use `_`,
`statement_control:sym<use>`; RakuAST names use `-`,
`statement-control:sym<use>`; slangs in the corpus are written against both).
Measured on Rakudo 2026.09: 692 + 861 names, 1124 in the union. It is
generated by a script that runs the `raku` oracle under `RAKUDO_RAKUAST=0` and
`=1`, writes one name per line, sorted, with a header naming the Rakudo
version, and is loaded once (lazily, on the first `define_slang` that has an
actions half) into a set.

A `make checks` guard keeps the file sorted and asserts every name in the
recognized-override map is in it (a recognized override that the table does
not list would be misclassified as a helper). Regenerating the file is part of
upgrading the `raku` oracle, like any other oracle-derived fixture.

This is data about Rakudo's grammar, not behaviour: no production is
implemented from it, and an entry only ever turns a would-be helper into a
handler — i.e. into "recognized, or a hard error".

### 2.3 The helper's body is not a requirement; the declarator's metaclass is

Nothing replaces the `ENTER` phaser `add_phaser` would install (§1.3): on a
non-precompiling implementation it is redundant with `BundleHOW.new_type`.
What Test::Async needs instead is #9926 item 1, scoped as follows:

- A role-kind slang declarator whose registration names a metaclass
  (`$*LANG.set_how('role', Test::Async::Metamodel::BundleHOW)` in the candidate)
  drives that metaclass's `new_type` when the role is declared, the way a
  class-kind one (`test-hub` → `HubHOW`) already does, with `callsame` reaching
  `Metamodel::ParametricRoleHOW.new_type` and returning the role being declared.
- The candidate's `:my $*NAME;` declarations with no initializer, or with a
  literal initializer (`:my $*TEST-RESTORE-PACKAGE := True;`), are read as part
  of the registration — exactly as ADR-0091 reads `:my $*PKGDECL := '...'` —
  and declared fresh around the protocol call, so `$*TEST-BUNDLE-TYPE :=
  callsame` binds the candidate's own dynamic. Declarations whose initializer
  needs a live parse (`self.package`, `HLL::Compiler.lineof(...)`) are not
  evaluated and not declared: a metaclass that reads one gets "Dynamic variable
  not found", naming it, which is the visible failure ADR-0091 §4 prefers over a
  guessed value.
- `$*W` stays unbound in mutsu. That is now load-bearing: the
  `unless $*W && $*W.is_precompilation_mode` branch is how the module chooses
  the registration path mutsu supports.
- The protocol call goes through the VM's callable path (`vm_call_on_value`
  and kin), not `call_method_with_values()`; AGENTS.md bans routing new
  features through that slow path. The class-side driver
  (`declare_drive_how_protocol`) still uses it and is existing debt that the
  same slice may migrate.

The grammar role's `method set_package(|)` (it restores the swapped role HOW)
stays ignored, as ADR-0091 §3 decided: mutsu's HOW swap is per keyword, so
there is nothing to restore.

## 3. Consequences

- `use Test::Async::Decl` activates; its three declarators register as they
  did under ADR-0091 before ADR-0098 broke it. `Config::BINDish::Test` gets
  past the same wall.
- After slice 1 (§2.1–2.2) alone, Test::Async still does not run: a scratch
  build that skips `add_phaser` loads `Test::Async::Decl`, then `use
  Test::Async` dies in its `EXPORT` on `for Test::Async::Hub.HOW.bundles`
  (#14290, a method's `nqp::list` result not HLL-ized), and the bundle list is
  empty where Rakudo's legacy frontend has one entry (§2.3, #9926). Both are
  separate slices with their own tests; the ecosystem record moves from
  `blocked_load` only when all three have landed.
- The rule matches Rakudo's own dispatch, so a helper method in any future
  actions role works without a new decision, while a genuine override mutsu
  does not implement still fails loudly.
- One more oracle-derived data file to regenerate on Rakudo upgrades. A stale
  table can only err in two directions: a removed production stays a handler
  (still loud), and a new plain-identifier production is read as a helper
  (the residual risk named in §2.1).

### 3.1 Implementation slices and their verification

1. **Classifier + table** (closes #13220). The ADR-0091 fixture
   (`t/lib/SlangDeclarator.rakumod`, already Test::Async::Decl-shaped) gains
   an actions-role helper, plus separate fixture slangs, so that an actions role
   carrying (a) a helper method called as
   `self.helper($/)` from a `package_declarator:sym<...>` action, which must
   activate; (b) a handler of a host production mutsu does not recognize
   (e.g. `blockoid`), and (c) an undeclared candidate of a known proto
   (`statement_control:sym<frobnicate>`), each of which must still fail
   naming the method. The fixture is valid Raku and is run under
   `RAKUDO_RAKUAST=0 raku` too. Real-module check: `use Test::Async::Decl;
   say "ok"` prints `ok`, matching legacy Rakudo.
2. **Role-kind declarator drives its HOW** (#9926 item 1, §2.3). Pinned by the
   fixture gaining a role-kind declarator with a `new_type`-overriding HOW that
   uses `callsame` and a candidate dynamic; real-module check:
   `Test::Async::Hub.HOW.bundles` has one entry after `use Test::Async::Base`,
   as under legacy Rakudo.
3. **#14290** (HLL-ize a method's `nqp::list` result), then the Test::Async
   suite against `RAKUDO_RAKUAST=0 raku` (27/28 there).

## 4. Alternatives considered (rejected)

- **Call-graph classification**: a method called as `self.m(...)` from a
  sibling method is a helper, everything else a handler. It fits
  Test::Async::Decl, but it is a heuristic over bodies that Rakudo does not
  use: a production handler that is also called from a sibling would be
  misread as a helper, and a helper reached any other way (`self."$name"`, a
  callback, dead code) would be a hard error. §2.1 is Rakudo's actual rule.
- **Drop the error for unknown actions methods** (treat every unrecognized
  actions method as inert). Silences `add_phaser`, but also silences a genuine
  override such as `statement_control:sym<if>`, leaving the unit parsing under
  a grammar the slang meant to change — the failure mode ADR-0026 §2.2 exists
  to prevent.
- **Recognize `add_phaser` by name**, or key on the module
  `Test::Async::Decl`: name-keyed provision, rejected by ADR-0026 §4 and
  ADR-0091 §4 on the same grounds, and by AGENTS.md's ban on test-specific
  hacks.
- **Execute the helper**: grow a `$*W` with `add_phaser` /
  `create_code_obj_and_add_child` and a QAST subset (`QAST::Block`,
  `QAST::Stmts`, `QAST::Op :op<callmethod>`, `QAST::WVal`) compiled to mutsu
  bytecode. This is ADR-0091's rejected "route 1", and §1.3 shows the module
  does not need it on an implementation without precompilation. It would also
  require binding `$*W`, which flips the module onto its precompilation path.
- **A hand-written production list** instead of a generated one: it would be
  a guess at Rakudo's grammar that drifts silently; the generated table is
  measured and regenerable.

## 5. Open questions

- Whether the ecosystem ledger should record a legacy-frontend baseline
  (`RAKUDO_RAKUAST=0`) for distributions written only for the legacy frontend,
  such as Test::Async: today they are `no_baseline` although a working oracle
  exists. That is a ledger policy question for the maintainer, outside this
  decision.
- A **grammar** role's plain methods are still read the ADR-0098 way (ignored
  unless a vocabulary map). By the same Rakudo rule, a grammar-role method that
  names a host grammar method (`set_package`) overrides it. Making that a hard
  error would reject Test::Async::Decl again; leaving it ignored is ADR-0091
  §3's decision for `set_package` specifically. Revisit if a corpus slang
  overrides a grammar *method* whose behaviour matters.
