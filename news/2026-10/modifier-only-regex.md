# A modifier-only regex matches the empty string

`/ :i /` and `/ :i :m /` used to die with "Null regex not allowed" because the null-regex check
ran after the leading internal modifiers had been stripped and saw an empty body. A body made only
of modifiers now parses to an empty pattern that matches the empty string, as in Rakudo, while a
truly empty regex (`/ /`, `/ a | /`) is still rejected. Fixes #11332.
