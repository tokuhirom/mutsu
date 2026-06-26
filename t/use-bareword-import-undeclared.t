use Test;

# A bare identifier as a positional import argument (`use Module Bar`) is a term
# reference, not an import symbol (those are strings / `<...>` / `:tags`). An
# undeclared one is X::Undeclared::Symbols, matching rakudo
# ("Undeclared name: Undeclared").

plan 3;

throws-like 'use DoesNotMatter Undeclared;', X::Undeclared::Symbols,
    'bare import arg is X::Undeclared::Symbols';

# Valid use forms still parse / load.
lives-ok { EVAL 'use lib "lib"' }, 'use lib "string" lives';
lives-ok { EVAL 'use Test' }, 'plain use Test lives';
