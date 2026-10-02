# Every use of the regex tree walk is counted

ADR-0135 retires the regex tree walk once the compiled backtracking engine covers everything, and
its deletion criterion was "the `regex-vm:` counter reads zero declined patterns". That counter
could not see most of what still runs the walk's code: a compiled pattern falls back to the walk
when the dynamic context keeps the engine out (`:my` lexicals of an enclosing regex, a rule's
dynamic declarations), a `<subrule>` call inside a compiled run bridges to the walk's producer
(arguments, `$*` parameters, wrapped tokens, left recursion, ...), single atoms such as
backreferences and lookarounds are matched by the walk's single-atom arm, and the entry point that
asks for every end at a position (`:ov`/`:ex`, LTM lookahead fates, cursor token methods) has no
compiled form at all.

`MUTSU_VM_STATS` now prints a second line that counts each of those uses with its reason:

```
[mutsu vm-stats] regex-walk: walked=2 (all-ends:match-all=2) bridged=1 (args=1) leaf=1 (backref=1)
```

`scripts/rx-decline-survey.sh` sums it across `t/` and the roast whitelist next to the pattern
counts, and ADR-0135 D7 and Slice E now read the walk's deletion off this line: `walked=` and
`bridged=` at zero, with every remaining `leaf=` reason moved out of the walk's modules. This is the
first step of [#10255](https://github.com/tokuhirom/mutsu/issues/10255); the slices that drive the
counts to zero follow.
