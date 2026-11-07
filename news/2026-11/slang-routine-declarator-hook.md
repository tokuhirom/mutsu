# Slang `routine-declarator` and `scope-declarator` productions run at statement start

A slang grammar role's new `routine-declarator:sym<...>` / `scope-declarator:sym<...>` token (and
the legacy underscore spellings) is now a production hook like `statement-control` (ADR-15762 slice
2, #13370): the parser asks the slang host at the start of a statement, the token runs on the host,
and the RakuAST node its action attaches stands for the statement. Still open: the same
productions in expression position (`my &f = fun { }`) and the `signature`/`trait` core-rule shims
that Slang::Mosdef and Qwiratry call.
