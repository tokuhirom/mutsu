# Five parse gaps off the head of the 99-distribution cluster

[#7988](https://github.com/tokuhirom/mutsu/issues/7988) grouped 99 distributions
by the only thing they had in common: mutsu described their parse failure by
dumping the set of things its parser would have accepted. [#8065](https://github.com/tokuhirom/mutsu/pull/8065)
fixed the diagnosis — every member now names the line the parser actually
stopped on — and its survey answered the ticket's real question: the cluster is
**a queue of small tickets, not a handful of large ones**, with a short head of
constructs that each reach two or three distributions through a shared
dependency.

This is the head, worked. Five gaps, each a construct rakudo compiles and mutsu
did not, each pinned against rakudo 2026.07.

## 1. A named parameter's alias could not be a Unicode identifier

```raku
submethod BUILD(:ν(:$!nu) = 1) { }     # Statistics::Distributions
```

mutsu supports Unicode identifiers everywhere else — variables, subs, classes,
methods — but the guard that decides whether `:name(...)` is an *alias* or a
*type constraint* tested the first **byte** for `is_ascii_alphabetic`. A UTF-8
lead byte such as `0xCE` is not ASCII-alphabetic, so `:ν(...)` fell into the
type-constraint/coercion path, which parses no trailing default. The defaulted
form failed outright and the undefaulted form silently lost the alias name:

```
sub f(:ν($nu) = 1) { }     Confused. … expected ')'
sub f(:ν($nu))    { }      parsed, then "Unexpected named argument 'ν'"
```

That is the same failure the uppercase case (`:ASTART($a) = 0`) already had,
reached by a different route. The guard now asks the identifier oracle
`is_raku_identifier_start` about the first **character**. Reaches
`Statistics::Distributions` and `Math::GameTheory`.

## 2. A pointy block could not take a signature-literal parameter

```raku
-> :(PrettyDump $pretty, $ds, Int:D :$depth = 0 --> Str) { … }   # PrettyDump
```

This reads like a signature literal but is not one: rakudo parses it as **one
parameter** — an anonymous named `$` whose sub-signature is the parenthesised
list — so the block's signature gists as `(:$ ($a, $b))`. mutsu's *sub*
parameter parser has always got this right; the pointy-block parameter parser
is a separate hand-rolled list of shapes and accepted `:` only when a sigil
followed, so the construct reached no branch at all and the whole block failed.

The pointy parser now delegates this one shape to the sub-parameter parser
rather than restating it — every other branch in that file is a hand-rolled
copy of one of its cases, and this is the one place a copy buys nothing.
`PrettyDump` goes `blocked_load` → loading cleanly; `Collection` and
`RakuConfig` depend on it.

## 3. `⚛-=` did not parse

`runtime::core_infix_names` already listed both spellings rakudo declares —
`⚛-=` with U+002D HYPHEN-MINUS and `⚛−=` with U+2212 MINUS SIGN — as operators
mutsu claims to know, but no parser site ever recognised either; only `⚛+=` was
handled, at three separate sites.

`⚛+=` and `⚛-=` are the same read-modify-write on the same target and differ
only in the sign of the delta, so both now lower to `__mutsu_atomic_add_var`
with the subtract forms negating their right-hand side through one shared
helper. The negation is applied to the delta expression, *before* the atomic
update runs, so atomicity is unchanged. Reaches `Async::Workers`,
`FFmpegProgressBar` and `Russian`.

## 4. `⚛=` could not initialize a declaration

```raku
my $qu ⚛= $!queue-unblock;     # Async::Workers
```

mutsu accepted the operator only in an assignment to an already-declared
variable. On a variable the declaration is only now creating there is nothing
to be atomic against — no other thread can hold a reference to it yet — so
rakudo treats this as an ordinary initialization, and so does mutsu now. Later
`⚛` operations on the name still go through the atomic machinery, exactly as
they do after a plain `my atomicint $i = 5`.

With 3 and 4 together, `Async::Workers::Job`, `::Worker`, `::CX`, `::Msg` and
`::X` all load; `Async::Workers` itself now stops only at a missing
`AttrX::Mooish`.

## 5. A bare type capture could not be followed by a return constraint

```raku
method add-enum-type(Str $name, ::Enum --> Promise) { … }   # Protocol::Postgres
```

The capture branch listed `)`, `]`, `,` and `;` as the things that may follow a
**bare** capture — one with no variable of its own — but not `-->`. The arrow
therefore fell through to the "anything after the capture is a parameter in its
own right" path, which tried to parse `--> Promise)` as a parameter. The arrow
belongs to the enclosing signature, exactly like the closing paren, so it joins
that list. `Protocol::Postgres` now parses (and stops at an unrelated
`Invalid typename 'EncodeBuffer'`); `Net::Postgres` depends on it.

## One head construct deliberately not fixed

`die "…":` (Math::FractionalPart, Astro::Utils, DateTime::Julian) is the
invocant-colon listop form: rakudo reads `die "boom":` as `"boom".die` and
raises `X::Method::NotFound`. mutsu does **not** implement those semantics for
any listop — it drops the colon and calls the listop normally, which is why
`say "hello":`, `sort @a:` and `return "r":` all "work" and `warn "w":` warns
instead of failing. `die` is the only one that additionally fails to *parse*.

Making `die` parse like its siblings would have traded a loud wrong answer for
a quiet one, so it is filed as
[#8141](https://github.com/tokuhirom/mutsu/issues/8141) with all five
measurements rather than half-fixed here.

## A note on method: `--dump-ast` is not a parse oracle

Probing a module file with `--dump-ast` does not run its `use` statements, so
any type the file imports is undeclared at the point the probe parses it.
`Async::Workers::Job` looked like a sixth gap (`when CX::AW::StopWorker {`)
until the error was compared with rakudo's: mutsu emits *exactly* rakudo's
message, word for word, for an undeclared type in that position. Loaded
properly with `-I`, the file parses. Two of the survey's remaining entries were
this artefact rather than gaps.

## Pins

All four files pass verbatim under rakudo 2026.07 as well as under mutsu.

- `t/routines/signature/unicode-named-param-alias.t` — 12 assertions, including
  the ASCII, hyphenated and uppercase alias spellings that already worked and a
  real type-constrained named parameter, which must still be a type.
- `t/routines/signature/pointy-block-signature-literal-param.t` — 9, including
  every other pointy parameter shape (ordinary, destructuring, named,
  sigilless, type-only).
- `t/routines/signature/signature-bare-type-capture-arrow.t` — 8, including
  every terminator that already worked.
- `t/concurrency/thread-lock/atomic-subtract-assign.t` — 13, including the
  U+2212 spelling, a compound right-hand side (the negation must bind the whole
  delta), and `⚛=` / `⚛+=` / `⚛++` / `--⚛` unchanged.
