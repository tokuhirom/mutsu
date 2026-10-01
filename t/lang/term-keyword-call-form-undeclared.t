use Test;

plan 3;

# `now` / `time` are CORE terms, not routines: with no routine of that name in
# scope, the call form is a compile-time "Undeclared routine", while a call
# form after whitespace is not the call form at all (#10369).

throws-like 'now()', X::Undeclared::Symbols,
    'now() without a declared routine is still an undeclared routine';
throws-like 'time()', X::Undeclared::Symbols,
    'time() without a declared routine is still an undeclared routine';

is (now (-) set(1)).^name, 'Set', 'now (-) $set is a set operation, not a call';
