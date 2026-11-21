# The package-chain miss path keeps the candidate `Symbol`

A bareword a method resolves through its enclosing packages
(`package_chain_var_fallback`) builds one qualified candidate per package
(`AttrX::Mooish::\Bool`, `AttrX::\Bool`, ...) as an interned `Symbol`, then
asked the `our` store and the env alias cascade for it by text. Each of them
interned the text again, and so did the cascade's last step,
`toplevel_package_symbol`. On AttrX::Mooish's `t/090-concurrent.custom` round
the candidates are almost all core type names (`Metamodel`, `Bool`, `Str`,
`Mu`, ...) that miss every store, so those re-interns were pure overhead:
~150 candidates and ~450 string-hash interns per round.

`get_our_var_sym` and `toplevel_package_symbol_sym` take the symbol, and the
chain passes it to `get_env_with_main_alias_sym`. The Mooish probe (callgrind
slope R=120→360, profiling build) went from 6,157,410 to 6,078,919
instructions per round (−1.27%). `t/modules/package-chain-term-binding.t` pins
the resolutions the chain answers and the core type names it must miss.
