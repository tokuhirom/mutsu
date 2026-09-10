use Test;

plan 5;

# Exporting the same symbol twice within one module raises X::Export::NameClash.

throws-like 'my module Expo { sub f is export { }; { sub f is export { } } }',
    X::Export::NameClash, symbol => '&f';

throws-like 'module M { sub g is export { }; sub g is export { } }',
    X::Export::NameClash, symbol => '&g';

throws-like 'module M { sub h is export { }; { sub h is export { } } }',
    X::Export::NameClash, message => /:s already been exported/;

# Distinct exported symbols do not clash.
lives-ok { EVAL 'module M { sub a is export { 1 }; sub b is export { 2 } }' },
    'distinct exported symbols do not clash';

# Multi candidates legitimately share a name when exported.
lives-ok {
    EVAL 'module M { multi sub f(Int) is export { 1 }; multi sub f(Str) is export { 2 } }'
}, 'exported multi candidates do not clash';
