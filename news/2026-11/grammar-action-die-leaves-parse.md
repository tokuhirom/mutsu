# A grammar action's `die` leaves `.parse`

An action that died when its rule reduced was parked in the pending-regex-error
slot while the match carried on. A parse that still matched then dropped the
error (`dispatch_package_parse` clears that slot on success, and so did the
`~~` a later action ran), so `Grammar.parse($text, :actions(...))` answered a
Match where rakudo throws. Config::TOML's duplicate-key checks
(`X::Config::TOML::InlineTable::DuplicateKeys`) relied on exactly that.

The error now has a slot of its own that later regex work does not clear, no
later action runs once one has died (rakudo unwinds at the `die`), and the
parse entry point raises it whether the match succeeded or not. Config::TOML's
19 test files now match rakudo.
