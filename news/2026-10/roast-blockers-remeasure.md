# Roast blocker ledger re-measured against Rakudo v2026.09

`TODO_roast/BLOCKERS.md` was re-measured in full for the first time since July: every
non-whitelisted roast file was run under a release build of mutsu (`c5896649`) and under the
local Rakudo, which is now v2026.09. `HISTORY.tsv` got a new row from `scripts/roast-history.sh`
(1262 / 1287 files, 193653 / 194402 subtests; the file total shrank with the roast re-vendor).

The whitelist is at 1426 / 1454. `6.c/APPENDICES/A04-experimental/01-misc.t` and
`S02-types/quanthash.t` have left the ledger; `S24-testing/6-done_testing.t` joined it as a
non-goal (it calls `ok` with a `:todo` named argument, which raku rejects too).

The re-measure mattered mostly for what moved backwards. Non-whitelisted files have no gate, so
general bugs had quietly turned them worse: `S05-mass/rx.t` no longer compiles because `^` after
another regex atom is rejected (#13470), `S12-meta/exporthow.t` no longer compiles because the
`class DECLARE::kw` spelling of an EXPORTHOW declarator is not read (#13471),
`6.c/MISC/bug-coverage-stress.t` now hangs on merged signal supplies and `exit` inside a signal
`whenever` (#13472), and `S10-packages/basic.t` loses `plan` in its last subtest (#13473).
Rakudo's upgrade also made `APPENDICES/A02-some-day-maybe/misc.t` a full raku pass, turning it
into an achievable target behind `my @a[;]` (#13469). Two more single-file blockers were narrowed
to minimal repros and filed: anonymous role punning (#13468, `MISC/misc.t`) and a pointy-block
sigilless param overwritten by a later `my $a` (#13474, `S32-basics/xxPOS.t`).

On the positive side, `S10-packages/basic.t` climbed from 59/83 to 74/83 and
`S02-names-vars/names.t` now runs to its end (153/156).
