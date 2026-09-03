# The language server keeps analysing past the first error — and stops dying on deep documents

ADR-0065 S3 was scheduled as "multiple diagnostics per document", which sounds
like a nicety: a file usually has one syntax error. The framing that matters is
the other one. **A document under edit is broken most of the time**, and a report
that goes quiet after the first failure hides everything below it. That also
makes this a prerequisite for S4: symbols and definitions are wanted *while* the
file is mid-edit, not only once it is complete.

## Recovery reuses the strict parser's diagnosis, not a lower tier

`parse_program`'s failure-rendering block — the one that produces the typed `X::`
message where an alternative diagnosed the input precisely, rakudo's
`.pre`/`.post` source context, and the hint — was extracted as
`render_parse_error`. A statement skipped by recovery is now diagnosed to exactly
that standard. Under D5 that matters more than the count: a second diagnostic of
lower quality would be worse than no second diagnostic.

It needed no offset arithmetic, which was the pleasant surprise. A `PError`'s
`remaining_len` measures the *shared buffer's* unconsumed tail rather than an
offset within whichever suffix the failing parser was invoked on, so a failure
raised inside `statement(rest)` already locates itself in the whole source.

The strict parse still produces diagnostic #1 — it stops at the first failure,
but its diagnosis of that failure is the richest available. Recovery then
re-parses and contributes what comes after, deduplicated by line.

## Cascade risk was measured rather than assumed

The obvious hazard is that skipping a statement leaves the parser mid-construct
and manufactures follow-on errors that are not real — the reason rakudo has a
sorrows/worries model. Over the 217 files of `modules/`, 11 fail to parse, **2
report more than one failure**, for 11 extra diagnostics. Inspecting those two:
every extra points at a distinct real construct on its own line, not at debris
from the previous skip.

Deduplication by line handles the one systematic repeat: the recovering pass
starts over, so its first failure is the strict parse's failure seen again. A
second failure on a line already accounted for is far more likely to be cascade
debris than a second defect, so the tie is broken toward saying less.

The undeclared-routine analysis deliberately does *not* run on a recovered parse.
Its false-positive direction inverts there — a skipped statement may have held
the very `sub` declaration that explains a later call — and
`stmt_list_partial`'s existing `note_partial_parse_skip` exists for exactly this
class of consumer.

## The survey overflowed its stack, which found a much worse bug

Before producing a single number, the corpus survey died with `fatal runtime
error: stack overflow`. That exposed a defect S1 had shipped: `mutsu-lsp` parsed
on the OS main thread.

mutsu's own CLI does not. `src/main.rs` spawns a 256 MB-stack thread because
grammar matching and nested expression parsing are deeply recursive. Measured on
a debug build with an 8 MB stack, `my $x = ((( … )))` **overflows at about fifty
nested parentheses**; twenty are fine. With the analysis stack, a thousand are
fine.

Fifty nested parentheses is not exotic — a nested data literal reaches it. And a
stack overflow *aborts the process*: `analysis::check`'s `catch_unwind`, which
turns a parser panic into a diagnostic, cannot rescue one. The whole session
would have died, every open document with it, on a file the CLI reads without
complaint.

The server now runs its loop inside `mutsu_lsp::on_analysis_stack`, and the
protocol tests spawn their server the same way, so a regression aborts the test
binary rather than passing quietly. The general lesson is worth stating plainly:
**any new front end for mutsu's parser inherits the CLI's stack requirement.** It
is a property of the parser, not of the CLI.

That is twice now that building the language server has found something wrong
with mutsu rather than merely consuming it — after S2's missing "Did you mean"
suggestions for a unit's own routines. D7 predicted the core-layer work would not
be an LSP-only tax; so far it has been the reverse.
