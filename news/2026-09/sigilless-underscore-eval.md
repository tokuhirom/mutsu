# A sigilless underscore survives EVAL

`my \_` now uses private storage instead of the environment key shared by the
topical `$_`, so an `EVAL` of `_` resolves the sigilless binding without
clobbering or being clobbered by topic save/restore.
