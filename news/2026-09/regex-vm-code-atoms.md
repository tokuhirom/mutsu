# The compiled regex engine runs `{ }`, `<?{ }>` and `:my`, and differential mode compares the code

The first part of ADR-0135's Slice C (#10253) compiles the atoms that run Raku code: a plain
`{ … }` block, a `<?{ … }>` / `<!{ … }>` assertion and a `:my` / `:our` / `:temp` declaration. Each
is a call-out op that runs the body on the caller's interpreter where the cursor reaches it, through
the same function the tree walk uses now (`regex_code_atom.rs`), so the two engines cannot drift on
when the code runs, what it sees or what it leaves behind. The body's compile is the cached one from
#10121, so there is no per-attempt compile and no new `Interpreter`.

`MUTSU_RX_DIFF=1` now compares the code as well as the match. Re-running the walk over a pattern with
code would run the user's code twice, so the compiled run records every invocation (the code, the
position, the captures it saw, the result it gave) and the walk replays them: its n-th invocation
must be the recorded n-th one. That is the order comparison ADR-0135 D6 asks for, without doubled
side effects.

The comparison found a bug in the walk. A non-capturing group, a branch or a quantified group gave
the walk a capture scope of its own, so `"abc" ~~ / a [ b { say $/.Str } ] c /` printed `b` where
rakudo prints `ab`, and `/ (a) [ b { say $0 } ] c /` could not see `$0` at all. A sub-pattern that
shares the regex's scope and holds code now sees the enclosing regex's captures and match start,
as one that holds a backreference already did (`t/regex/syntax/regex-code-atom-capture-scope.t`).

Two shapes keep the walk because the compiled form would hide the enclosing captures from the code:
code inside a `%` quantifier (`separator-code`) and inside a `&` branch (`conjunction-code`).

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), the `code` declines fell
from 406 to 83 and compiled patterns went from 5,981 to 6,319. What is left of Slice C is
`<{ … }>`, `** {n}` and `<$var>` / `<@var>` / `$( … )` interpolation.
