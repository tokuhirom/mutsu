# Side effects in a subrule call's arguments reach the caller's lexicals

`<x($c++)>` and `<x({ $calls++; 2 }())>` in a grammar token now update the
caller's `$c` / `$calls`, as rakudo does. Two layers dropped the write: the
argument evaluation (`eval_regex_expr_value`) discarded its swapped-in env, and
`eval_token_def` restored its saved env after the pre-match argument
instantiation. The first now publishes the free-variable writes the way an
assertion body does; the second re-applies those logged writes to the restored
env (skipping the token's own parameters). Closes #10612.
