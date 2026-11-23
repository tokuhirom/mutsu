# Deliberate divergences from rakudo

mutsu's oracle is rakudo (`raku`, currently 2026.11) together with roast. This
page lists every place where mutsu **deliberately** behaves differently and keeps
doing so. A divergence here is a decision, not a gap:

- A bug mutsu still has, or a feature it lacks, belongs in an issue, `PLAN.md` or
  `TODO_roast/BLOCKERS.md`, not here.
- **Add a new deliberate divergence to this page in the PR that makes it.**
  Write a minimal repro with both outputs, the reason, and links to the issue,
  ADR or PR that records the decision. Remove an entry once mutsu changes to match
  rakudo.
- An ecosystem test file that fails only because of one of these is graded
  `accepted` through `ecosystem/accepted-divergences.toml`
  ([ADR-0130](adr/0130-ecosystem-accepted-divergences.md)). Section E lists those
  files.

Outputs marked "raku" are from rakudo 2026.11 (MoarVM) and outputs marked
"mutsu" are from `main`, unless an entry says otherwise. "Rakudo artefact" means
behaviour that follows from how rakudo or MoarVM is built, which neither roast
nor raku-doc specifies.

## A. Security and robustness

### A1. Analysis never runs code

`raku -c` runs `BEGIN` blocks and loads the modules a `use` names. mutsu's
analysis surfaces (`src/analysis/`, the language server, `--dump-*`) never run
code: no `BEGIN`, no parse-time probe, and no slang activation.

- Reason: security. Opening or checking a file must not execute it. See the
  user decision of 2026-10-23.
- Refs: [docs/security.md](security.md),
  [ADR-17741](adr/17741-slang-package-declarators-run-on-the-host.md),
  [docs/language-server.md](language-server.md), AGENTS.md "Keep the trust
  boundaries".

### A2. No `.precomp` directory next to the source

rakudo writes its precompilation cache into `lib/.precomp` (or
`<root>/.precomp`). mutsu keeps its cache in the user's own `0700` cache
directory.

- Reason: security. A checkout cannot plant a cache entry that would then be
  executed, and the on-disk layout is a rakudo artefact. This is the maintainer
  decision of 2026-11-08, option (c) of ADR-16451.
- Refs: [ADR-16451](adr/16451-filesystem-repository-precomp-directory.md),
  [ADR-11756](adr/11756-compiled-bytecode-precompilation.md),
  [#16451](https://github.com/tokuhirom/mutsu/issues/16451), [docs/security.md](security.md).

### A3. Deep recursion raises a catchable exception

```raku
sub f { f() }; f(); CATCH { default { say .^name } }
```

- raku: no fixed limit. MoarVM grows the call stack on the heap until memory
  runs out, and the process is killed.
- mutsu: `X::AdHoc` ("Too deep recursion"), raised when native stack headroom
  runs low.
- Reason: safety. Untrusted nested input must not abort the process, and mutsu's
  VM runs on the Rust stack.
- Refs: [ADR-0100](adr/0100-deep-recursion-raises-on-native-stack-headroom.md),
  AGENTS.md hard rules.

### A4. Racy programs do not crash the VM

Under unsynchronized concurrent mutation of one container, rakudo can crash with
a MoarVM oops. mutsu stays memory-safe and answers instead, although a race can
still corrupt the program's own logical state, as on rakudo. A `+=` / `++` on a
shared scalar is deterministic in mutsu, although raku does not promise that it
is atomic.

- Reason: memory safety
  ([ADR-12814](https://github.com/tokuhirom/mutsu/issues/12814); the
  concurrency model otherwise follows rakudo).
- Refs: news/2026-10/param-container-concurrent-reads.md
  ([#11210](https://github.com/tokuhirom/mutsu/issues/11210)),
  [docs/gc-post-3a-roadmap.md](gc-post-3a-roadmap.md).

### A5. Two threads requiring each other's module do not hang

```raku
# lib/CycA.rakumod: unit module CycA; sleep 0.3; require CycB;
# lib/CycB.rakumod: unit module CycB; sleep 0.3; require CycA;
use lib 'lib';
say await start { require CycA; 'a' }, start { require CycB; 'b' };
```

- raku: hangs (each thread waits for the other's load).
- mutsu: dies `circular module dependency detected: CycA -> CycB -> CycA`.

A module loads once even when two threads ask for it at once: the second thread
waits for the first one's load to finish (ADR-17216's per-module load claims).
A thread whose wait would close a cycle of such waits does not wait. It loads
as it would alone, which for a program that genuinely needs both modules from
both sides reaches the same circular-dependency error as a single thread.

- Reason: robustness. A deadlock gives the user nothing to act on. The program
  is circular, and the error names the cycle.
- Refs: `src/runtime/module_load_claims.rs`,
  [#17216](https://github.com/tokuhirom/mutsu/issues/17216).

## B. Roast or the documentation chosen over rakudo

### B1. A `Str` never holds a surrogate code point

```raku
say chr(0xD800).ord;
```

- raku: `55296`
- mutsu: dies `X::InvalidCodepoint: Invalid surrogate codepoint 55296`
- Reason: roast `S29-conversions/ord_and_chr.t` has `dies-ok {chr(0xD800)}`,
  which rakudo carries as `#?rakudo todo`, and S15 agrees. The proposal to keep
  surrogates was rejected (maintainer decision 2026-10-21).
- Refs: [ADR-12640](adr/12640-a-str-holding-a-surrogate-is-a-codepoint-body.md)
  (Rejected), [#14416](https://github.com/tokuhirom/mutsu/issues/14416),
  [#12640](https://github.com/tokuhirom/mutsu/issues/12640). Ecosystem
  consequences are in section E.

### B2. Binding through a run-time name into a package stash works

```raku
our $p = 'OUR'; class A { our $x = 1 }
$::($p)::A::x := my $y; $y = 7; say $A::x;
```

- raku: the general forms (`$::($p)::A::x := v`, `::($p)::A := v`,
  `&::($p)::f := &g`) are refused with "Cannot bind to pseudo-package" or "This
  case of binding is not yet implemented". The simple case above happens to
  print `7`.
- mutsu: `7`, and every form binds.
- Reason: roast `S02-names/pseudo-6c/6d/6e.t` specifies these binds, todo'd for
  rakudo. mutsu follows roast.
- Refs: [#18771](https://github.com/tokuhirom/mutsu/pull/18771),
  [#18742](https://github.com/tokuhirom/mutsu/issues/18742),
  news/2026-11/run-time-name-bind.md, `t/vm/scope/pseudo-our-indirect-bind.t`.

### B3. Roast tests written against open rakudo bugs pass

mutsu passes, in full, several roast files that rakudo v2026.07 fails because
of rakudo bugs that are still open:

| File | mutsu | rakudo v2026.07 |
| --- | --- | --- |
| `S05-capture/caps.t` | 56/56 | 43/56 |
| `S05-metasyntax/charset.t` | 90/90 | 57/90 |
| `S05-metasyntax/regex.t` | 68/68 | 58/68 |
| `S05-modifier/ignorecase.t` | 115/115 | 103/115 |
| `S02-literals/numeric.t` | 89/89 | SORRY |
| `S32-str/sprintf-a.t` (`%a`/`%A`) | 586/586 | 0/586 |

- Reason: roast is the specification.
- Refs: news/2026-09/roast-revendor-2026-09.md,
  news/2026-09/roast-revendor-fully-recovered.md (rakudo issues 4105, 5588,
  2962, 4512 and 6524).

### B4. `X::Comp`'s `.pre` / `.post` use the correct eject point

mutsu reports the position the parser actually rejected. rakudo reports a
different point, a bug that roast `S32-exceptions/misc.t` carries as
`#?rakudo todo` (rakudo issue 4431).

- Refs: news/2026-08/comp-exception-pre-post-and-two-missing-attributes.md.

### B5. Where rakudo disagrees with its own documentation, mutsu follows the docs

- `Type/Int` `polymod` (lines 134 and 150).
- Argument-less `.first` (`Type/Any:817`).
- The return type of `minpairs` / `maxpairs` (`Type/Any:617,628`).
- `readchars` read-ahead (`Type/independent-routines:473`,
  `Type/IO/CatHandle:162`).
- mutsu implements `goto`, so `FOO: goto FOO` really loops
  (`Language/perl-func:778`); rakudo has no `goto`.

- Reason: raku-doc is a specification source. Revisit an entry only if roast
  pins rakudo's behaviour.
- Refs: [docs/doc-diff-backlog.md](doc-diff-backlog.md),
  news/2026-09/doc-diff-sweep-2026-09-27-117-issues.md.

### B6. `CALLERS::` / `DYNAMIC::` follow 6.e

```raku
sub c { say CALLERS::<bar> }; sub d { constant bar = 5; c() }; d();
```

- raku (default 6.d): `5`. A non-dynamic `my` variable reads as
  `(LoweredAwayLexical)`. Under `use v6.e.PREVIEW`, rakudo dies with "Cannot
  access 'bar' through CALLERS, because it is not declared as dynamic".
- mutsu: dies `X::Caller::NotDynamic` in every language version.
- Reason: rakudo's 6.d and 6.e answers differ. mutsu follows 6.e, which roast
  `S02-names/pseudo-6e.t` specifies.
- Refs: [#17846](https://github.com/tokuhirom/mutsu/issues/17846),
  [#17892](https://github.com/tokuhirom/mutsu/pull/17892).

### B7. Deleting the last element shortens the array

```raku
my @g = 1, 2; @g.DELETE-POS(1); say @g; say @g.elems;
```

- raku: `[1 (Any)]`, then `2`. The `:delete` adverb form also leaves 2 elements
  (rakudo 2026.07).
- mutsu: `[1]`, then `1`.
- Reason: roast `S32-array/delete-adverb.t` (whitelisted) requires a trailing
  delete to shorten the array.
- Refs: [#10985](https://github.com/tokuhirom/mutsu/issues/10985).

## C. Rakudo artefacts and quirks mutsu does not copy

### C1. A block's operator `multi` sees the scope's outer lexicals

```raku
for 1..3 -> $k {
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() * $k }
    print (2 + 3), ' ';
}
```

- raku: `0 0 0`, each with "Use of uninitialized value $k of type Any in
  numeric context".
- mutsu: `5 10 15`

On rakudo an ordinary `multi`, a `sub` and a `my multi` declared in the same
block all see each iteration's `$k`, in both implementations. Only an
**operator** candidate does not on rakudo. It is reached through the
block-scoped operator dispatcher that rakudo derives, and that dispatcher holds
the uncloned (static) candidate, whose outer lexicals are never set. mutsu treats
an operator candidate like any other `multi`.

A consequence that follows the same rule: `{ my $offset = 100; multi
infix:<+>(UInt $a, UInt $b) { callsame() + $offset }; say 1 + 2 }` recurses
until "Too deep recursion" in mutsu. rakudo prints `3` only because its
candidate never sees `$offset`.

- Reason: rakudo artefact. No roast test pins it (`S06-multi/*`,
  `S04-declarations/*`). The maintainer decided on 2026-11-22 to keep mutsu's
  behaviour.
- Refs: [#18729](https://github.com/tokuhirom/mutsu/issues/18729).

### C2. A `.wrap` installed after a call site was compiled is honoured

```raku
use nqp;
sub small(int $n) { nqp::add_i($n, 1) }
sub use-small(int $n) { small($n) }
say "before-wrap=", use-small(1), ",", use-small(2);
&small.wrap(sub ($n) { callsame() * 10 });
say "after-wrap=", use-small(1), ",", use-small(2);
```

- raku: `before-wrap=2,3` then `after-wrap=2,3`. rakudo's static inliner keeps
  the unwrapped callee.
- mutsu: `before-wrap=2,3` then `after-wrap=20,30`, with or without TRIR
  inlining (`MUTSU_TRIR_INLINE=0`). An inlined region bails out when its callee
  has been wrapped.
- Reason: rakudo optimizer artefact. Wrapping is documented to affect later
  calls.
- Refs: [#18732](https://github.com/tokuhirom/mutsu/pull/18732),
  [#8673](https://github.com/tokuhirom/mutsu/issues/8673),
  `t/vm/codegen/adr8673-trir-inline.t`.

The same rule covers a grammar token. If a program parses once, then wraps the
token (`G.^find_method('word').wrap(...)`) and parses again, rakudo's second
parse ignores the wrapper (`True / True / 0` in the issue's repro), while mutsu
runs it (`True / True / 3`). `Type/Routine.rakudoc` says `.wrap` "in-place
modifies the routine", which makes rakudo's answer an optimisation artefact
([#9189](https://github.com/tokuhirom/mutsu/issues/9189)).

### C3. `Supply.stable` drops a value still pending at `done`

```raku
my $s = Supplier.new; my @c;
$s.Supply.stable(0.01).tap({ @c.push: $_ }, done => { @c.push: 'done' });
$s.emit(1); $s.done; sleep 0.3;
say @c;
```

- raku: `[done 1]`. With a short window, rakudo's cued emit outlives the
  supply's `done` and delivers after it.
- mutsu: `[done]`. `done` passes through at once and the held value is dropped.
- Reason: an emit after `done` breaks the Supply protocol. mutsu's derived
  supplies stop delivering once done, so delivering would mean emitting on a
  finished supply through every forwarder. Both implementations agree that
  `done` arrives at once and nothing is flushed ahead of it.
- Refs: [#18708](https://github.com/tokuhirom/mutsu/pull/18708),
  [#18615](https://github.com/tokuhirom/mutsu/issues/18615).

### C4. `substr-rw` stores splice into the live string

```raku
my $s = "hello"; my $y := substr-rw($s, 0, 1); my $z := substr-rw($s, 4, 1);
$y = "J"; $z = "O"; say $s;
```

- raku: `hellO`. Each Proxy's STORE splices into the string it captured, which
  undoes the other write.
- mutsu: `JellO`
- Reason: neither the documentation nor roast specifies the store side, and
  copying rakudo would silently discard writes. The maintainer chose to keep
  mutsu's behaviour.
- Refs: [#15734](https://github.com/tokuhirom/mutsu/issues/15734),
  news/2026-10/substr-rw-live-store-divergence.md,
  `t/regex/subst/substr-rw-store-splices-live-value.t`.

### C5. Holes in an `is default` array read the default

```raku
my @m is default(7) = 1, 2, 3; @m[0]:delete; say @m.sum;
```

- raku: `5`. `Array.sum` skips the hole, and `.Array` dies on one.
- mutsu: `12`. Every view reads the default.

An `is default(Nil)` array renders a deleted slot as `Any`, where rakudo shows
`Nil`.

- Reason: rakudo artefacts.
- Refs: [#10360](https://github.com/tokuhirom/mutsu/issues/10360),
  [#10317](https://github.com/tokuhirom/mutsu/issues/10317),
  news/2026-10/array-default-holes-every-view.md.

### C6. `Code.line` / `Code.file` report the real source position

- A `WhateverCode` (`* + 1`) reports its own line and file. rakudo reports line
  1 and a null `Str` file, whose `.raku` dies.
- A `multi method` dispatcher reports its first candidate's line. rakudo points
  into the metamodel.
- A native routine (`&infix:<+>`) answers `Nil` for both. rakudo gives a
  `SETTING::` path that mutsu has no source for and will not fake.
- A bare `/.../` literal answers `Nil` where rakudo gives line 1 and a null file.

- Reason: compilation-unit artefacts, and mutsu's answers are more useful.
- Refs: news/2026-08/code-line-file-reflection.md,
  news/2026-08/code-line-file-regex-token.md.

### C7. `enum … does Role` lists the role in `.^roles`

```raku
role Weird { method w { 1 } }; enum Flags does Weird (A => 1, B => 2); say Flags.^roles;
```

- raku: `()`
- mutsu: `((Weird))`
- Reason: rakudo's `EnumHOW` artefact. Reporting the composed role is the more
  useful answer, and nothing depends on the empty one.
- Refs: news/2026-08/enum-does-role-accepts-not-dispatched.md.

### C8. Smaller quirks not copied

| Case | rakudo | mutsu | Source |
| --- | --- | --- | --- |
| `$a ~~ array:U` (literal smartmatch) | `True` | `False`, as `(array:U).ACCEPTS($a)` is on both | news/2026-08/native-type-smiley-and-element-check-gaps.md |
| `class A { method m { $x }; my $x = 7 }` | the internal `(LoweredAwayLexical)` marker | the class static | news/2026-10/class-body-my-shadows-outer-lexical.md |
| `BEGIN { my $x = 1 / 0; $x.Int }` | leaks `X::Numeric::DivideByZero` | `X::Comp::BeginTime`, like every other BEGIN error | news/2026-08/begin-rat-divzero-wrapping-verified-not-worth-fixing.md |
| `Complex.Rat(eps)` / `.FatRat(eps)` with a non-negligible imaginary part | an accidental "Too many positionals passed" | `X::Numeric::Real`, like the zero-argument forms | news/2026-10/complex-rat-fatrat-epsilon.md |
| A `gather` that reads an element it has not taken yet | hangs | reports the cycle | news/2026-10/gather-self-reference-iterator.md |
| `when` on a non-match (e.g. `$_ = Any; when 2`) | `0`, an artefact of one smartmatch lowering | a real `Bool`; the `0` is kept only for the two shapes roast and `t/` pin | [ADR-0052](adr/0052-a-when-clause-produces-its-value-on-the-stack.md) |
| Regex LTM: literal length inside a quantified subrule; `:m` literals | rakudo NFA quirks | the literal-length chain ends at a quantifier; `:m` literals rank like `:i` ones | [ADR-0022](adr/0022-regex-alternation-ltm-ranking.md) |
| `(* + 1 xor * + 2).WHAT` | `Nil` with a "Useless use" warning | `(WhateverCode)` | [ADR-0033](adr/0033-whatever-priming-leaf-and-derived-scope.md) |
| RakuAST of a parenthesised `repeat … until` condition | a `.not` that double-negates, so rakudo skips the loop | no `.not` | news/2026-10/rakuast-bare-loop-term.md |
| `sprintf("%s", $obj)` | calls the user `.Str` twice | calls it once | [docs/treewalk-method-removal-plan.md](treewalk-method-removal-plan.md), `t/routines/dispatch/sprintf-user-str-dispatch.t` |
| An exported nested routine's static frame | a write shows up in the next activation (MoarVM artefact) | every activation starts fresh | [ADR-0132](adr/0132-nested-routine-exports-install-at-enclosing-routine-registration.md), [#10114](https://github.com/tokuhirom/mutsu/issues/10114) |
| `nqp::p6takefirstflag` in a `for` body with a `FIRST` phaser | `1` on the first iteration (code-generation artefact) | not mirrored | [ADR-11505](adr/11505-rakudo-frame-state-nqp-ops.md) |
| `nqp::eqaddr(nqp::hllize($l), nqp::hllize($l))` | `0` (a fresh wrapper per call) | `1`, with equal `.WHICH`es | [ADR-12666](adr/12666-raw-vm-collections-are-a-view-of-shared-storage.md) |
| NativeCall type identity | separate type objects per qualified name | qualified in display only (`NativeCall::Types::Pointer`); one registry key | [ADR-0056](adr/0056-nativecall-types-display-only-qualification.md) |
| `truncated-to`, `first-date-in-month`, `last-date-in-month` on a `Date` subclass | dies `P6opaque: no such attribute '$!day'` | answers | [ADR-11276](adr/11276-built-in-methods-are-handler-rows.md) |
| `IO::Path.split($s, 1)` | a `List` | a `Seq` | ADR-11276 |
| `Array.push(1)` on the type object | `X::Multi::NoMatch` | `X::Assignment::RO` | ADR-11276 |
| `--profile-kind` and friends without `--profile` | value ignored | validated | [docs/profiler.md](profiler.md) |
| An unknown production handler in slang actions | silently never called | a hard error | [ADR-13220](adr/13220-slang-actions-role-helper-methods.md) |
| Bag/Mix set operators (`∪`, `∩`, `∖`, …) on huge weights | BigInt weights | saturate at `i64` (assignment and coercion keep BigInt) | news/2026-06.md |
| `CORE::uc("x")` | the internal "Cannot find method 'is_dispatcher' on object of type BOOTCode" | the pseudo-stash error "No such method 'CALL-ME' for invocant of type 'Nil'" | [#13162](https://github.com/tokuhirom/mutsu/issues/13162), [#17852](https://github.com/tokuhirom/mutsu/pull/17852) |
| A backtrace's bottom `<unit>` frame (`try two()` on line 3, `say $!.backtrace.list.tail.line` on line 4) | `4`: the line is read lazily | `3`, the line of the call | [#16770](https://github.com/tokuhirom/mutsu/issues/16770) |
| `try { my Int $x = "a" }; $!.raku` | "No such method 'raku' for invocant of type 'ContainerDescriptor'" | `X::TypeCheck::Assignment.new(symbol => "\$x", got => "a", expected => Int)` | [#16654](https://github.com/tokuhirom/mutsu/issues/16654) |
| A role-body `my regex` in a module file that interpolates a role lexical | "Cannot resolve caller INTERPOLATE_ASSERTION" | works | [#12600](https://github.com/tokuhirom/mutsu/issues/12600) |
| `class C { method m { has $!g //= 5; $!g } }; say C.new.m` | "Cannot modify an immutable Nil value" | `5` | [#13329](https://github.com/tokuhirom/mutsu/pull/13329), `t/oo/method/has-decl-nested-in-method.t` |
| `sub a { CALLER::LEXICAL::<$_> }; for 1 { say a().raku }` | `Any` | `1` | [#15552](https://github.com/tokuhirom/mutsu/pull/15552) |
| `(1+1i, 2+2i ... 5+5i)` | `…, <3+2i>, <4+2i>, <5+2i>` (Complex step-deduction quirk) | `…, <3+3i>, <4+4i>, <5+5i>` | [#17012](https://github.com/tokuhirom/mutsu/pull/17012) |
| `multi s(*%m, *@a) {"m-a"}; multi s(*@a) {"a"}`, then `s(1)` and `s()` | `a`, then `m-a` (declaration-order quirk) | `X::Multi::Ambiguous` for both | [#10817](https://github.com/tokuhirom/mutsu/pull/10817) |
| `(1,2).rotor` | an internal "Cannot resolve caller infix:«<»()" | "Cannot resolve caller rotor(List:D: )" | [#17914](https://github.com/tokuhirom/mutsu/pull/17914) |
| `splice`'s `X::Multi::NoMatch` candidate list | shows `is revision-gated("6.e")` | does not | [#17914](https://github.com/tokuhirom/mutsu/pull/17914) |
| An aliased `<foo=?after x>` | a non-empty match | zero-width | [#9137](https://github.com/tokuhirom/mutsu/pull/9137) |
| `my $x := subbuf-rw($b, 0, 1); $x = Buf.new(9); say $x` on `Buf.new(1,2,3)` | `Buf:0x<01>` (a quirk of its Proxy) | `Buf:0x<09>` | [#10798](https://github.com/tokuhirom/mutsu/pull/10798) |
| RakuAST of `True` after `my \True = 42` | still a `Term::Enum` | not mirrored | [#18567](https://github.com/tokuhirom/mutsu/pull/18567) |
| RakuAST Pod: a configured block right after another block | comes first in the statement list | source order; roast `S26-documentation/04-code.t` differs for this chain of quirks | [#14880](https://github.com/tokuhirom/mutsu/issues/14880), [#18354](https://github.com/tokuhirom/mutsu/pull/18354), [#18360](https://github.com/tokuhirom/mutsu/pull/18360) |
| `buf32.new(0x11223344, 0x55667788).read-ubits(0, 8)` and the other wide-buffer bit offsets | `0x11223344` (leaked MoarVM pointer arithmetic) | not mirrored | [#6816](https://github.com/tokuhirom/mutsu/pull/6816), news/2026-08/wide-buffer-bit-accessor-divergence-left-unfixed.md |
| A routine that runs while a block-scoped `use` is live and executes on the caller's env | does not see the imported names | may see them (only widens visibility) | [ADR-11136](adr/11136-module-global-merge-is-lexical-to-the-importing-scope.md) §4 |
| `GLOBAL::OuterCls` for a class a module merged lexically | not resolvable | resolvable, as mutsu's `GLOBAL` is the shared store (only widens visibility) | ADR-11136 §4 |

### C9. Temporary: module `BEGIN`s run on every load

rakudo runs a precompiled module's `BEGIN` blocks once, at precompilation. mutsu
runs them on every load until precompiled bytecode exists. This entry goes away
when that lands.

- Refs: [ADR-0134](adr/0134-begin-time-prologue.md),
  [ADR-11756](adr/11756-compiled-bytecode-precompilation.md),
  [#18012](https://github.com/tokuhirom/mutsu/issues/18012).

### C10. A custom `ClassHOW` subclass keeps role state per instance

```raku
class H is Metamodel::ClassHOW does Metamodel::DefaultParent { }
H.new.set_default_parent_type(Int); say H.new.has_default_parent_type;
```

- raku: `1`. The second `H.new` appears to share the first one's role state.
- mutsu: `0`. Each instance has its own attributes, which is the documented
  role semantics.
- Reason: this is probably an artefact of how rakudo's `Metamodel::ClassHOW.new`
  answers. It was recorded as an accepted divergence on 2026-11-03.
- Refs: [#16239](https://github.com/tokuhirom/mutsu/issues/16239) (item 2),
  [#16246](https://github.com/tokuhirom/mutsu/pull/16246).

### C11. An imported `proto` / `multi` merges with the program's

Module `MP2` exports `proto greet(|) { say "M proto"; {*} }` and
`multi greet(Int $x)`:

```raku
multi greet(Str $x) {}; use MP2; say &greet.candidates; say &greet.file;
```

- raku: `(Int)` only, then `.file` dies ("concatenate requires a concrete
  string, but got null"). When either side's proto has a body, rakudo builds an
  only-star dispatcher with no `file`. That drops either the program's
  candidates or the proto body. With a program-side proto, rakudo gives a
  compile error instead ("Calling greet(Str) will never work with (Int $x)").
- mutsu: `(Str) (Int)` and the program's file. The candidate lists merge, and
  the proto body is kept.
- Reason: there is no rakudo behaviour here worth reproducing. mutsu's merge is
  the consistent form of rakudo's only-star rule (closed as not planned,
  2026-11-22).
- Refs: [#18587](https://github.com/tokuhirom/mutsu/issues/18587).

### C12. `.^methods` leaves out rakudo's internal hooks

`Map.^methods` lists 54 methods on rakudo and 41 on mutsu, and `Pair.^methods`
lists 30 and 25 (2026-10-21). The names mutsu does not list are:

- `FLATTENABLE_*`, `POPULATE`, `PUSH_FROM_MAP`, `STORE_AT_KEY`;
- `store-odd-number`, `IterationBuffer`, `TEMP-LET-*`, `GRAB_ONE`;
- `reification-target`, `INSTANTIATE-GENERIC`, `from-slurpy*`, `set-shape`,
  `is-generic`;
- `Pair.abs-value` and `Pair.add-value`.

- Reason: these are rakudo-internal hooks with no user-observable contract, so
  they stay unlisted by design.
- Refs: [#13494](https://github.com/tokuhirom/mutsu/issues/13494).

## D. Implementation identity

### D1. mutsu identifies as itself

- `$*RAKU.compiler.name` is `mutsu`. Only `.version` is a rakudo release
  coordinate ([ADR-0104](adr/0104-compiler-version-is-a-rakudo-release-coordinate.md)).
- `$*VM.name` is `mutsu`, never `moar`, and no MoarVM `$*VM.version` is
  reported (user decision 2026-09-25,
  [#12829](https://github.com/tokuhirom/mutsu/issues/12829),
  [#15266](https://github.com/tokuhirom/mutsu/issues/15266)).
- No `raku`-named executable is installed
  ([#15807](https://github.com/tokuhirom/mutsu/issues/15807), user decision
  2026-11-03).
- `$*RAKU.compiler.verbose-config` has a different key set, and
  `CompUnit::Repository` chains lead with `file#`.
- MoarVM-only knobs are not emulated: `MVM_SPESH_LOG`
  ([#16953](https://github.com/tokuhirom/mutsu/issues/16953)) and the
  `MVM_COVERAGE_LOG` line table
  ([#15323](https://github.com/tokuhirom/mutsu/issues/15323)).

### D2. Bundled modules are not hidden from `require`

mutsu bundles modules such as Cro::HTTP::Client, IO::Socket::SSL and
HTTP::UserAgent. A test that probes `require` and skips when the module is
absent therefore runs its real flow on mutsu, where the oracle rakudo skips it.

- User decision 2026-11-03
  ([#15525](https://github.com/tokuhirom/mutsu/issues/15525)).

### D3. Scheduling order between tasks is not rakudo's

mutsu does not copy rakudo's lazy thread-pool activation, cold-pool FIFO order,
or its `Lock`'s lack of hand-off. A test that relies on one task winning an
unordered race can resolve differently. rakudo also loses those races under
load.

- Refs: [ADR-15967](adr/15967-start-blocks-submission-order.md),
  [ADR-0105](adr/0105-promise-resolution-dispatches-through-the-promise-scheduler.md),
  [#17724](https://github.com/tokuhirom/mutsu/issues/17724),
  [#17566](https://github.com/tokuhirom/mutsu/issues/17566),
  [#16898](https://github.com/tokuhirom/mutsu/issues/16898).

### D4. Rakudo-specific internals are not provided

- The `MoarVM::` namespace is not provided (user decisions 2026-10-23 and
  2026-10-31; `ecosystem/README.md`, `ecosystem/exclude.txt`,
  [#13511](https://github.com/tokuhirom/mutsu/issues/13511),
  [#12609](https://github.com/tokuhirom/mutsu/issues/12609),
  [#8733](https://github.com/tokuhirom/mutsu/issues/8733)).
- rakudo's hidden, deprecated, undocumented core `from-json` is not added.
  `sub f() { start from-json("x").list }` compiles on rakudo and is
  `Undeclared routine: from-json` on mutsu
  ([#15808](https://github.com/tokuhirom/mutsu/issues/15808)).
- Rakudo-Type-Introspection is not supported (user decision 2026-11-08, which
  replaced the 2026-11-03 decision to implement all of it). The revision-gated
  `CORE::v6c` / `v6d` / `v6e` namespaces, `CORE::v6c::PseudoStash` and
  `.^is-implementation-detail` are not provided
  ([#15616](https://github.com/tokuhirom/mutsu/issues/15616)).
- `Rakudo::Version` gates on `$*RAKU.compiler.name eq 'rakudo'`, so it does not
  work on mutsu (D1, [#8945](https://github.com/tokuhirom/mutsu/issues/8945)).

### D5. RakuAST trees omit nodes rakudo's BEGIN-time machinery adds

`visit-children` does not reproduce three compile-time shapes:

- `$.a` is not `Var::Attribute::Public` over an `ApplyPostfix` chain.
- A method has no implicit `self` / `%_` `Signature`.
- An attribute default is a `Trait::WillBuild`, where rakudo has a
  `Method::Initializer`.

- Reason: out of scope (decision 2026-10-31). Reproducing them would mean
  generating compile-time artefacts that nothing consumes.
- Refs: [#14820](https://github.com/tokuhirom/mutsu/issues/14820) (rows 6, 7
  and 9).

### D6. `zprintf` is a mutsu extension

mutsu has a `zprintf` routine, and rakudo has neither the routine nor a method.
The routine stays, with its own tests. It has no method form.

- Refs: [ADR-11276](adr/11276-built-in-methods-are-handler-rows.md),
  `t/oo/method/zprintf-no-method.t`.

## E. Ecosystem test files graded `accepted`

Each of these files fails on mutsu only because of a divergence above, or
because of a race or artefact the project decided not to reproduce. The exact
failure shapes are in `ecosystem/accepted-divergences.toml`
([ADR-0130](adr/0130-ecosystem-accepted-divergences.md)).

| Distribution / file | Why | Issue |
| --- | --- | --- |
| App::RaCoCo `t/02-ext-proc`, `t/15-misc`, `t/19-configuration` | no `raku` alias (D1) | [#15807](https://github.com/tokuhirom/mutsu/issues/15807) |
| Code::Coverage `t/01-basic` | MoarVM coverage-log line artefacts (D1) | [#15323](https://github.com/tokuhirom/mutsu/issues/15323) |
| Concurrent::BoundedChannel `t/01-sanity` | `Lock` hand-off race (D3) | [#17566](https://github.com/tokuhirom/mutsu/issues/17566) |
| Datastar::SDK `t/02-signals` | surrogate in a `Str` (B1) | [#14416](https://github.com/tokuhirom/mutsu/issues/14416) |
| Dev::ContainerizedService `t/clickhouse.t` | bundled module runs its real flow (D2) | [#15525](https://github.com/tokuhirom/mutsu/issues/15525) |
| DSL::Shared `t/Array-of-regexes-matches` | rakudo mixes `Match::CachedCompiledRegex` into an interpolated `Str` in place, changing its `.WHICH` | [#9746](https://github.com/tokuhirom/mutsu/issues/9746) |
| Jupyter::Chatbook / Jupyter::Kernel `t/05-autocomplete` | surrogates in a `Str` (B1) | [#14416](https://github.com/tokuhirom/mutsu/issues/14416) |
| Linux::NFTables `t/02-oo` | libnftables double free in `DESTROY`; rakudo passes only because MoarVM never collects the object before exit | [#17834](https://github.com/tokuhirom/mutsu/issues/17834) |
| MCP::Server `t/21-elicitation-dispatch` | cold-pool task order (D3) | [#17724](https://github.com/tokuhirom/mutsu/issues/17724), [#14684](https://github.com/tokuhirom/mutsu/issues/14684) |
| NativeHelpers::Blob `t/02-cstruct` | `$*VM.name` is not `moar` (D1) | [#12829](https://github.com/tokuhirom/mutsu/issues/12829) |
| Object::Delayed `t/02-basic` | thread-timing race (D3) | [#16898](https://github.com/tokuhirom/mutsu/issues/16898) |
| ParaSeq `t/08-grep`, `t/20-unique` | a race in ParaSeq 0.2.7 itself | [#17723](https://github.com/tokuhirom/mutsu/issues/17723) |
| Rakudo::Options `t/02-with` | `MVM_SPESH_LOG` (D1) | [#16953](https://github.com/tokuhirom/mutsu/issues/16953) |
| SOAP::Client `t/05-simple-service`, URI::FetchFile `t/060-http-useragent` | bundled module runs its real flow (D2) | [#15525](https://github.com/tokuhirom/mutsu/issues/15525) |

A related policy, which is not a behaviour divergence: a distribution that does
not work on rakudo's RakuAST frontend (legacy `$*W` / QAST slangs) does not have
to work on mutsu (AGENTS.md, user decision 2026-10-21).
