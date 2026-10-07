# A user `subset` attribute default is checked at construction, not declaration

`has DevicePath $.p = '/dev/i2c-1'` with `subset DevicePath of Str where { .IO ~~ :e }` used to die
while the class was being declared ("Can never assign default value ..."), because the
declaration-time default check evaluated the subset's `where` against the literal. Rakudo cannot
decide a refinement at compile time (its predicate may depend on the run-time environment), so the
class declares fine and `.new` raises `X::TypeCheck::Assignment` when the default fails. mutsu now
skips the static check for user subsets, and the native default constructor enforces the subset
predicate on defaulted values too. Found via `RPi::Device::PiGlow` (blocked_load), whose two test
files now pass.
