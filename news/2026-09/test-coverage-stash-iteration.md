# Package stashes now iterate as key/value pairs

Package `Stash` values now participate in list context as their visible symbol
table entries. This covers list assignment, `.list`, `.pairs`, `.kv`, and
`.grep`, so `Test::Coverage` 0.0.8 loads unchanged and its `t/01-basic.rakutest`
passes under mutsu with all 8 assertions matching Rakudo.
