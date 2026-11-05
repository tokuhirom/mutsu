# A Method object called with a spread Capture keeps its invocant

`$method(|c)`, where `c` is a sub's slurpy Capture holding the caller's variable, ran the method
with `self` = Nil: an `^find_method` object keeps an anonymous invocant slot, which
`function_invocant_arg` did not recognise as the implicit invocant, so the cell never became
`self`. It now does. JSON::RPC's server (`$candidate( |params )`) needed this for notifications.
