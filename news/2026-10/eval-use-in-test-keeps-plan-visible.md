# `eval-lives-ok 'use Module'` no longer hides Test's `plan` from the script

`eval-lives-ok` runs its `EVAL` inside the Test compunit, so a `use` there loaded the module
with Test as the importer and scoped Test's package-less families to Test alone. The script's
own earlier `use Test` had never been recorded (the family had no scope record yet), so a later
`plan` died with `Unknown function: plan`. Imports of a family that is not scoped yet are now
remembered and seed the record when the family is scoped (#13473); `roast/S10-packages/basic.t`
gets its last subtest back.
