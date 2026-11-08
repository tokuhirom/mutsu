# Slang tokens can call the core `<statementlist>` rule

A slang token that matches `'{' <statementlist> '}'` now gets a `RakuAST::StatementList` for the
statements the Rust parser reads at the cursor, the way `<block>` and `<signature>` already work
(ADR-15762 §2.3, ADR-16888). It is the back-call a wrapper of the core `statementlist` method will
delegate to; the wrapper itself (a user `method statementlist` that runs around the parser's list
loop, as EEL's does) is the next step of ADR-16888.
