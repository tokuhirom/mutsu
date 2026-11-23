# A lexical type's package-chain probe skips the alias cascade

A bareword type read checks whether a lexical (`my class`) type of that name is
in scope, and its miss path walks the enclosing package chain asking for
`Pkg::\u{1}Name` under each package. Each step ran the whole
`get_env_with_main_alias_sym` cascade. Only two of its probes can hold such a
key: the running compunit's file-scope lexicals (a module's `my class`) and
the env. The package chain now asks just those two, in the cascade's order,
and a debug assertion checks that the full cascade agrees.

The AttrX::Mooish t/090 probe drops by 0.59% Ir per round (#18274).
