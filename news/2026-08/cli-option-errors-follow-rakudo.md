# A command-line option error follows rakudo

mutsu's CLI treated any argument it did not recognise as the program file, so a
typo'd switch produced a nonsensical error and a malformed negation exited
non-zero:

```
$ mutsu --nosucharg=foo foo.raku
Could not open --nosucharg=foo. Failed to stat file: no such file or directory   # exit 1
$ mutsu -/hv
SORRY! Option '-/hv' cannot be negated                                           # exit 1
```

Two roast files pin this through `Test::Util`'s `is_run`
(`S19-command-line/arguments.t` test 6, `S19-command-line-options/04-negation.t`
tests 2 and 3). Neither names a status, but `is_run` defaults one to 0 whenever
`err` is absent or empty — so both require **exit 0**. They passed only because
mutsu's *native* reimplementation of `is_run` did not apply that default.

Measured against rakudo 2026.06, the rule is narrower than "rakudo exits 0 on
errors": an error *parsing the option list* is status 0, while a program-level
failure — including a program file that cannot be opened — is not. mutsu already
agreed on the missing-file case; it disagreed only about option parsing.

So mutsu now:

- rejects an unrecognised option instead of taking it for a filename —
  `Illegal option --name` for a long one (named without its `=value`),
  `No such option -x` for a short one, each followed by the usage text, all on
  stderr, exit 0;
- exits 0 from a malformed negation, keeping the message on stdout, which is
  what `04-negation.t` asks for (rakudo prints its own to stderr and fails that
  file — the block is `#?rakudo todo ''`);
- honours `--` as end-of-switches, so a program file may begin with a dash;
- still exits 1 when the program file cannot be opened.

The cost is stated plainly in the ADR: `mutsu --typo file.raku` now exits 0, so a
script checking `$?` cannot distinguish a typo'd switch from a clean run. That is
a known rakudo wart, adopted knowingly and contained to option parsing — every
other failure path keeps its non-zero status. The reasoning, the measurements,
and the rejected alternatives are in
[ADR-0017](../../docs/adr/0017-cli-option-errors-follow-rakudo.md).

A methodological note worth repeating: the first pass at this measurement read
`rc=0` for *every* rakudo invocation, because the commands were piped into
`head` and the shell reported `head`'s status. The real table only appeared once
each command was run without a pipeline. Measure the thing you are actually
asking about.

Pin: `t/cli-option-errors.t`. With this in,
`todo/tickets/retire-native-test-util-overrides.md`'s residue drops to 2 files.
