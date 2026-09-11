# `CURSOR_MORE`: walking every match through the NQP cursor protocol

`Match.^lookup("CURSOR_MORE")` resolved to nothing, so the second half of the
NQP cursor protocol was unavailable to ecosystem code. `String::Utils`'
`replace-all` is written entirely in it and died with
`No such method 'CALL-ME' for invocant of type 'Nil'`
([#7931](https://github.com/tokuhirom/mutsu/issues/7931)). The first half —
`!cursor_init` plus calling a `Regex` on a cursor — landed in
[#7883](https://github.com/tokuhirom/mutsu/issues/7883); this completes it.

## What the protocol asks for

lizmat's house style for "walk every match without re-scanning the prefix" is
to drive the regex engine by hand rather than through `~~`:

```raku
my constant $cursor-init = Match.^lookup("!cursor_init");
my constant $global      = Match.^lookup("CURSOR_MORE");

my $cursor := $needle($cursor-init(Match, $haystack, :0c));
my int $pos = nqp::getattr_i($cursor, Match, '$!pos');
while $pos >= 0 {
    ...                                    # $!from .. $!pos is one match
    $cursor := $global($cursor);
    $pos = nqp::getattr_i($cursor, Match, '$!pos');
}
```

rakudo's `Cursor.CURSOR_MORE` re-invokes the cursor's own `$!regexsub` on a
fresh un-started cursor placed just past the last match. Two details of that
were measured against rakudo 2026.07 and are load-bearing:

- the resumption position is `$!pos`, **bumped by one when the last match was
  zero-width** (`$!from == $!pos`). Without the bump, `/x*/` finds the same
  empty match forever and `replace-all` never terminates.
- the fresh cursor is un-started (`$!from == -1`), so the re-invocation *scans*
  forward rather than anchoring, and a run that finds nothing more yields the
  ordinary failed cursor (`$!pos == -3`) the caller's loop condition tests for.

Both traces now agree with the oracle exactly, including the zero-width walk
that is the reason the bump exists:

| regex / haystack | `[$!from,$!pos]` per step |
| --- | --- |
| `/o/` on `"foo boo"` | `[1,2] [2,3] [5,6] [6,7] [7,-3]` |
| `/\d+/` on `"a12b345"` | `[1,3] [4,7] [7,-3]` |
| `/x*/` on `"axb"` | `[0,0] [1,2] [2,2] [3,3] [4,-3]` |

## How mutsu remembers the regex

mutsu's `Match` has no `$!regexsub` slot, so it grew one: the internal
attribute `CURSOR_REGEXSUB_ATTR`, holding **the callable that produced the
cursor**, stamped on the result of every cursor-protocol regex call. Storing
the callable rather than a pattern string means `CURSOR_MORE` re-invokes it
through the same `Regex.CALL-ME` path the original `$needle($cursor)` call
took, so an `rx:i//`'s adverbs — and any later fix to how they are honoured —
are inherited instead of reconstructed. The stamp preserves the `Match`
instance id, because a cursor is handed straight to user code.

The lazy `Match` node (ADR-0016 P5) answers `None` for the attribute without
forcing, alongside the other post-hoc keys, so a probe for it on an ordinary
match costs nothing.

## The surface this deliberately does not claim

`CURSOR_MORE` resumes a cursor produced by a `Regex` called on a cursor. Two
narrower surfaces are left out, and it says so rather than guessing:

- **an ordinary `"abc".match(/b/)`.** In rakudo every `Match` *is* a spent
  `Cursor` and carries its `$!regexsub`, so `CURSOR_MORE` works on one. Here
  the stamp would have to go on every match the engine produces, and because it
  rebuilds the `Match` eagerly that would cost ADR-0016 P5's laziness on the
  hottest path in the interpreter — for a surface no known consumer reaches
  for. Making it free instead means threading the invoked regex down to the
  engine entry points, which is the same objection the `cursor_class` stamp
  already records, and worth its own issue if a consumer ever needs it.
- **a grammar token method called on a cursor.** Its result feeds the
  custom-HOW subrule side channel by instance identity, so stamping the
  regexsub on it would perturb a path unrelated to this protocol.

Also unchanged: `.^lookup`'s return value is a plain mutsu `Method`, where
rakudo answers an `NQPRoutine` for `!cursor_init` and a
`Method+{is-implementation-detail}` for `CURSOR_MORE`. Both are callable, which
is all the idiom uses; mutsu has no NQP-level routine type to report.

## Result

The real, vendored `String::Utils` now runs its `replace-all` verbatim under
mutsu with output identical to rakudo's:

```
replace-all("foobarfoo", /foo/, "X")   # XbarX
replace-all("a1b22c333", /\d+/, "-")   # a-b-c-
replace-all("aaa", /a*/, "<>")         # <><>
```

Pinned by `t/regex/match/match-cursor-more.t` (24 assertions, every
expectation measured against rakudo 2026.07, including the verbatim
`String::Utils::replace-all` body), next to `match-cursor-protocol.t` from
#7883.

`String::Utils`' own `t/01-basic.rakutest` is still blocked earlier than this,
at a separate parse gap on line 33 (`is (root <abcd abce abde>), "ab", ...` — an
imported listop followed by a `<...>` word quote, filed as
[#7939](https://github.com/tokuhirom/mutsu/issues/7939)), so its record does
not move yet.
