# `<$var>` regex interpolation rejected by over-broad "dangerous code" heuristic

## Affected tests

- `t/http-request-serializer.rakutest` (Cro::HTTP dist) — tests 16 and 17
  ("multipart/form-data ..."), the `:rx` branch of `is-request`:
  `like $joined-output.decode('utf-8'), /<$expected-output>/, $desc;`
  dies `Prohibited regex interpolation`. This is the SECOND blocker of the file,
  reachable only after the map/`when` succeed bug is fixed (see
  `tap-map-grep-inline-block-swallows-succeed.md`); with both fixed the file
  passes 1..17.

## Repro

`tmp/tapdiag-rxinterp.raku` (verified 2026-08-09):

```raku
my $p = Q/'boundary="' $<b>=[<-["]>+] '"'/;
my $s = 'Content-type: multipart/form-data; boundary="abc123"';
say ?($s ~~ /<$p>/);
```

- raku: matches (True; `$<b>` = `abc123`)
- mutsu: `Prohibited regex interpolation` (X::SecurityPolicy), exit 1

## Root cause

`<$var>` interpolation compiles the string as regex source
(src/runtime/regex_parse_core.rs:2520-2589). Before compiling it calls
`contains_dangerous_regex_code`
(src/runtime/regex_parse_modifier.rs:725-772) and raises
`make_security_policy_error` (regex_parse_modifier.rs:847-856) on a hit
(raise sites: regex_parse_core.rs:2560-2566 and
src/runtime/regex/regex_match_capture.rs:359).

The heuristic rejects plainly legal regex source:

- any `<$` / `<@` substring (line 728) — but nested interpolation is legal raku;
- `$(` / `@(` (line 732);
- ANY `{` or `}` (line 736) — also hits `**{2..3}`-style quantifiers and other
  non-code uses;
- the double-quote check (lines 744-756): split on `"`, flag a `$`/`@`/`%`/`&`
  inside an odd chunk. The Cro pattern above contains `"` characters inside
  single-quoted literals (`'boundary="'`), so the "inside quotes" parity is
  meaningless, and the `$<b>=` named-capture alias lands in an odd chunk → flagged;
- `<\w+(...)>` subrule-with-args, `:my `/`:our `, `:(`.

Rakudo's actual rule: interpolated regex source is compiled and matched; only
*code execution* inside an interpolated regex (`{ ... }` code blocks,
`<{ ... }>`, `<?{ ... }>`/`<!{ ... }>`) is prohibited without
`use MONKEY-SEE-NO-EVAL` (X::SecurityPolicy::Eval). Named captures, char
classes, quoted literals with any characters, and nested `<$x>` are all fine.

## Fix direction

Narrow `contains_dangerous_regex_code` to constructs that would EXECUTE code
when the interpolated string is compiled:

1. Tokenize just enough to find, outside quoted literals (`'...'` / `"..."`)
   and char classes (`<[...]>`, `<-[...]>`):
   - a bare `{` opening a code block,
   - `<{`, `<?{`, `<!{`,
   - `<::(` dynamic lookup (keep),
   - `:my ` / `:our ` (keep — these declare in the caller's scope).
2. Drop the `<$`/`<@`, `$(`/`@(`, double-quote-chunk, and `<\w+(...)>` checks
   entirely, or gate them the same way (they must not fire inside quoted
   literals / char classes). `$<name>=` is a capture alias, never code.
3. Optional completeness: implement `use MONKEY-SEE-NO-EVAL` to allow even the
   code cases when the pragma is active, and name the exception
   `X::SecurityPolicy::Eval` with Rakudo's message ("Interpolation of a
   variable that contains code ..."), so `throws-like` tests keyed on the type
   still pass.

Keep `contains_longname_alias` (regex_parse_modifier.rs:779) as is — separate
check, not implicated.

Risks: the heuristic is also consulted for match-time interpolation
(regex_match_capture.rs:359); apply the same narrowed check there. Some roast
security tests may pin the current over-broad behavior — run
`grep -rl "Prohibited regex interpolation\|X::SecurityPolicy" t/ roast-whitelist.txt`
before/after and adjust only mutsu-local `t/` pins, never roast.

## Verification

- The repro above matches with `$<b>` = `abc123` under mutsu.
- `t/http-request-serializer.rakutest` tests 16-17 pass with the real `like`
  regexes (probe already showed the serialized bytes are correct: with the
  regex check stubbed to a `.contains('multipart/form-data')` check the file
  passed 1..17, so this fix is the last measured blocker of the file — though
  the exact `$<b>` boundary back-reference matching in the full pattern is
  exercised for the first time by the real run).
- Existing regex interpolation tests: `t/` files matching `rx-interp*` /
  security-policy pins, plus a new `t/regex-interp-capture-alias.t` covering
  `$<name>=[...]`, `<-["]>`, quoted `"` literals, and a genuine `{ code }`
  rejection.
