# The parser's line numbers were quadratic in the source, and a scratch interpreter still seeded an env it never read

Round 16 of [#7576](https://github.com/tokuhirom/mutsu/issues/7576). Instructions
retired on the ticket's 60-row YAMLish document go **1,983,139,212 ->
1,841,091,693 (-7.16%)**. Attribution is callgrind Ir throughout (deterministic
and load-independent), with the module precompilation cache warmed before each
profiled run per round 14's note.

Round 15 handed off "the profile is flat: `LocalKey::with` 7.3% across a dozen
callers, allocator ~19%, `memcpy` 3.9%, largest single mutsu function 1.85%",
with one named item (`resolve_parsed_token_candidates_in_pkg` interning its
package name) worth ~0.7%. A fresh `callgrind_annotate --tree=caller` found
three larger things instead, none of them in the regex matcher and none of them
visible under their own name.

## (a) `current_line_number` rescanned the whole source prefix, per call (-3.07%)

`$?LINE` — and every parse warning, every `X::Comp` location, every
`test-assertion-line` record — resolves a parser position to a line number by
counting the newlines that precede it. It did so by reconstructing the prefix as
a `&str` and calling `.matches('\n').count()`: **O(offset) per lookup**, and
therefore quadratic in the source length. The parser asks once per statement
*attempt*, backtracking included, so parsing the 26 KB `YAMLish.rakumod` made
**347,485** of those lookups, scanning an average of several kilobytes each.

`set_original_source` now builds the newline offsets once (the parse already
reads every byte) and each lookup is a `partition_point`. The heredoc
`LeakedRegion` path — whose line numbering jumps at the terminator, so it counts
newlines in `[jump_offset, offset)` rather than in a plain prefix — carries the
same index, expressed as a difference of two `partition_point`s. `next_match`
inclusive **63,315,136 (3.19%) -> 2,441,692 (0.13%)**, `memchr_aligned`
**34,747,454 -> 1,592,214**. Pinned by `t/lang/quoting/line-number-after-heredoc.t`
(verified against rakudo 2026.07).

This one is not specific to YAMLish: it is paid by every mutsu parse, and its
cost grows with the square of the file.

## (b) The grammar dynamic-variable scan built one `String` per character (-1.03%, and -2.6% of the program's allocations)

`.parse` establishes a grammar's parse-wide `:my $*/@*/%*NAME` declarations by
scanning every rule pattern for them. `collect_dynamic_var_decls` walked *every
character position* of the pattern and, at each one, materialized the entire
remaining pattern as an owned `String` in order to `strip_prefix(":my ")` it —
quadratic again, for an answer that is "no declarations" for almost every rule.
It now walks the `:` positions with `str::find` and tests borrowed slices.

Two things around it went with the same change: the scan ran **twice over every
pattern** (once to collect the declarations, once to build the per-rule map that
gives each match of a declaring rule its own binding), and the registry walk that
collects the patterns spelled its prefix test `k.resolve()` — an owned `String`
per `token_defs` key per `.parse` call, the exact shape round 11 (a) removed from
the proto-variant scan. `collect_dynamic_var_decls` inclusive **25,652,327
(1.29%) -> 0** (it no longer appears in the profile at all). Pinned by
`t/grammar/grammar-dynvar-decl-scan-with-adverbs.t`, which puts regex adverbs,
`:sym<…>` proto variants and a multi-byte literal in the scanner's path.

## (c) A scratch interpreter still seeded the process magicals (-2.69%)

Rounds 10 and 12 stopped a regex/grammar scratch interpreter from building the
built-in registry, from re-scanning the bundled-battery directory, and from
running `init_io_environment`. What survived is that `Interpreter::new` still
built `$*PID`, `$*TZ`, `@*ARGS`, `$*INIT-INSTANT` and `$*SCHEDULER` on every
construction — a `getpid`, a `localtime_r`, a `SystemTime::now`, a
`make_instance` and five `String` keys — and **every one of the ten scratch
construction sites spells `Interpreter { env: <the caller's env>, .. }`**
(directly, or through `make_regex_eval_env`, which starts from
`self.env.clone()`), so all of it was dropped unread. `%*ENV`'s OS sweep was the
only part already behind the guard. This document builds **1,609** scratch
interpreters.

Two smaller process constants went the same way: the built-in encoding table
(~30 `String`s per construction) is a shared `Arc` template now, like the
built-in registry, safe for the same reason — `register_encoding` goes through
`cow_table_mut`, so the first interpreter to add a user encoding forks itself a
private copy. And `precomp::enabled_by_default` read `MUTSU_PRECOMP` — an
environment lock plus a `String` — once per construction; it is a process-wide
switch, so it is read once.

`Interpreter::new` inclusive **93M (4.7%) -> 64.2M (3.49%)**;
`make_instant_from_posix` **3,566,796 -> 0**; `std::env::_var` **3,252 calls ->
1,644**. Pinned by `t/regex/regex-code-block-process-magicals.t`, which checks
that all six magicals still resolve inside an embedded regex code block —
through the caller's env, which is where they were always coming from.

## Method note

Round 12's lesson was "a profile's flat tail regenerates itself; re-run the
caller attribution after every round rather than carrying the previous round's
*what is left* forward as a conclusion." Round 16 is that lesson twice over, and
sharpens it in one direction: **the tail regenerates from a different subsystem
than the one you have been working on.** Rounds 10-15 each found their item in
the regex engine or the interpreter constructor; two of round 16's three are in
the *parser*, which no previous round had profiled at all, because the module
parse hides behind `LocalKey::with`, `malloc` and `memchr` exactly the way every
earlier round's item hid behind `malloc` and `memcpy`. The tell was the same
ratio-in-one-line as rounds 11 and 12: `CharSearcher::next_match` entered
347,485 times under a thread-local accessor, in a program that parses one 1,116-
line module.

The corollary is procedural: a `--separate-callers=2` run is what turned
"`next_match` is called from `LocalKey::with`" (useless — every thread-local
access is) into "`next_match` is called from `LocalKey::with` called from
`stmt_list_with_mode`". One extra callgrind run, and worth reaching for whenever
the immediate caller of a hot leaf is a generic wrapper.

## What is left

Re-measured rather than carried forward: the profile is flat again
(`LocalKey::with` 7.6% across a dozen callers, allocator ~20%, `memcpy` 4.2%),
and `Symbol::intern` is its largest single caller at 427,652 calls. The one item
with a measured mechanism is new, and it is not in this benchmark's inner loop at
all: **`use YAMLish` costs 324 M instructions, 17.6% of the whole run, and 209 M
of that is the parser re-parsing the module's source to collect its exported
names** while a precompiled AST for exactly that file already sits in the on-disk
cache that the *runtime* load consults. Filed separately as
[#8095](https://github.com/tokuhirom/mutsu/issues/8095), because caching the
scan result on disk has a transitive-invalidation question (a scan harvests
names from the modules it recursively scans) that wants deciding before code.
