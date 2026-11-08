# A slang can declare its own helper rules; `package-declarator:sym<>` is a declarator

Two walls of the real EEL slang (#16888): a colon-free rule a slang's grammar role declares is its own
helper when rakudo's grammar lacks a rule of that name (`src/parser/stmt/simple/core_grammar_rules.txt`
lists the 2026.09 ones), whether or not another rule calls it; and `token package-declarator:sym<kw>`
(the RakuAST grammar's spelling) adds a package declarator like `package_declarator:sym<kw>`. EEL
now activates and runs its `routine-declarator:sym<pattern>` token, and stops at rakudo's grammar
helpers (`key-origin`, `package-def`, `kok`, `set_braid_from`, `DECLARATOR`).
