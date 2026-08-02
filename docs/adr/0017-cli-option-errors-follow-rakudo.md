# ADR-0017: A command-line *option* error follows rakudo — message, stream, and a zero exit status

- **Status**: Accepted
- **Date**: 2026-08-02
- **Deciders**: tokuhirom, Claude
- **Related**: `src/main.rs` (`illegal_option`, `print_negation_error`, the argument loop),
  `t/cli-option-errors.t`, `roast/S19-command-line/arguments.t`,
  `roast/S19-command-line-options/04-negation.t`,
  `todo/tickets/retire-native-test-util-overrides.md`.

## 1. Context

mutsu's CLI treated any argument it did not recognise as the program file. So

```
$ mutsu --nosucharg=foo foo.raku
Could not open --nosucharg=foo. Failed to stat file: no such file or directory
$ echo $?
1
```

and a malformed negation exited non-zero:

```
$ mutsu -/hv
SORRY! Option '-/hv' cannot be negated
$ echo $?
1
```

Two roast files assert on this, through `Test::Util`'s `is_run`:

- `S19-command-line/arguments.t` test 6 — `is_run(Str, :args['--nosucharg=foo', 'foo.raku'],
  { out => '' }, 'Unknown options do not spit warnings to stdout')`
- `S19-command-line-options/04-negation.t` tests 2 and 3 — `:args['-/hv']` and
  `:args['--/target', 'foo']`, each expecting
  `out => rx/"SORRY" .+ "cannot be negated"/, err => ''`

Neither names a status, but `is_run` supplies one:

```raku
%expected<status> = 0 if
    not %expected<status>:exists
    and (not %expected<err>:exists or %expected<err> ~~ Str and %expected<err> eq '');
```

so both files require **exit 0**. They passed only because mutsu's *native*
reimplementation of `is_run` did not apply that default; under the real
`Test::Util` module they fail. That is the immediate trigger — these are two of
the files blocking `todo/tickets/retire-native-test-util-overrides.md`.

## 2. What rakudo actually does

Measured with rakudo 2026.06. **Measure the exit status directly** — an earlier
pass through this read `rc=0` everywhere because the command was piped into
`head`, and the shell reported *`head`'s* status:

| invocation | stdout | stderr | status |
| --- | --- | --- | --- |
| `raku nosuchfile.raku` | — | `Could not open nosuchfile.raku. Failed to stat file: …` | **1** |
| `raku --nosucharg=foo foo.raku` | — | `Illegal option --nosucharg` + usage | **0** |
| `raku --zzz` | — | `Illegal option --zzz` + usage | **0** |
| `raku -z` | — | `No such option -z` + usage | **0** |
| `raku -/hv` | — | `Grouped options '-/hv' contain '/', …` + usage | **0** |

The rule is narrower than "rakudo exits 0 on errors": an error *parsing the
option list* is status 0, and a program-level failure — including a program file
that cannot be opened — is not. mutsu already agreed on the missing-file case
(status 1); it disagreed only about option parsing.

## 3. Decision

Follow rakudo for option-parsing errors:

- an unrecognised option before the source is an **option error**, not a
  filename: `Illegal option --name` for a long option (naming it without its
  `=value`), `No such option -x` for a short one, followed by the usage text,
  **all on stderr**, exit status **0**;
- a malformed negation keeps mutsu's existing `SORRY! Option '…' cannot be
  negated` wording **on stdout** and now also exits **0**;
- `--` is honoured as end-of-switches, so a program file may begin with a dash;
- a program file that cannot be opened is unchanged: message on stderr, exit
  status **1**.

The negation message stays on stdout deliberately. rakudo prints its own to
stderr and therefore *fails* `04-negation.t` — the whole block is marked
`#?rakudo todo ''`. The roast file is the spec here, and it asks for the message
on stdout with an empty stderr; mutsu already satisfied that half and there is no
reason to regress to rakudo's implementation wart when the spec text disagrees
with it.

## 4. Consequences

- `roast/S19-command-line/arguments.t` and
  `roast/S19-command-line-options/04-negation.t` pass under the real
  `Test::Util`, taking that ticket's residue from 4 files to 2.
- An unknown switch is now rejected instead of being silently taken for the
  program file, which is a real usability gain: `mutsu --dump-bytcode foo.raku`
  used to report "Could not open --dump-bytcode".
- **The cost, stated plainly: `mutsu --typo file.raku` exits 0.** A shell script
  that checks `$?` cannot tell a typo'd switch from a successful run. This is a
  known rakudo wart and we are adopting it knowingly, because the alternative is
  to diverge from the reference implementation on the exact behaviour two spec
  tests pin. It is contained: it applies only to option parsing. Every other
  failure path — parse errors, runtime errors, test failures, an unopenable
  program file — keeps its non-zero status.
- If this bites in practice, the reversal is a two-line change in
  `illegal_option` / `print_negation_error`, and it would have to be recorded as
  a superseding ADR together with whatever happens to the two roast files.

## 5. Alternatives considered

- **Keep exit 1 and leave the two roast files failing.** Rejected: they are
  spec tests, and leaving them failing also leaves the `Test::Util` provider
  retirement blocked on something that is not a bug in the provider.
- **Keep exit 1 and special-case `is_run`.** Rejected outright — that is a
  test-specific hack, which the project bans.
- **Exit 0 for *every* CLI error, including a missing program file.** Rejected:
  rakudo does not do that (status 1, measured above), and it would throw away a
  genuinely useful signal for no spec benefit.
