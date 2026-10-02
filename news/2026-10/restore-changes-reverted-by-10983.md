# Restore three changes that #10983 reverted by accident

The branch behind #10983 (RakuAST sub traits) was squashed with
`git reset --soft origin/main` after `origin/main` had moved past the branch's
real base. The squashed commit therefore carried the branch's old tree on top
of a newer `main`. Merging it silently undid the three PRs merged in between:

- #10963 — `MAIN` usage messages in rakudo's format (`src/runtime/main_usage.rs`,
  `t/tooling/main-usage-format.t`);
- #10844 — sigil-alias regex variable calls in their other forms
  (`src/runtime/regex/regex_alias_subcap.rs`,
  `t/regex/regex-sigil-alias-var-call-other-forms.t`);
- #10964 — documenting the container-capture edge of ADR-0032.

This change reapplies exactly the diff between the branch's base (`12576ff8`)
and the `main` it was squashed onto (`fca694de`). Every restored file is
byte-identical to its state before the bad merge, except `src/runtime/mod.rs`,
which also keeps the later changes it has received since. The squash procedure
now resets onto the branch's merge base, never onto a freshly fetched
`origin/main`.
