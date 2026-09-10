use Test;

plan 5;

# A placeholder variable used directly in an attribute initializer cannot be
# captured by any signature -> X::Placeholder::Attribute.
throws-like 'class RT78112 { has $.a = $^b + 1; }', X::Placeholder::Attribute,
    placeholder => '$^b';

throws-like 'class C { has $.a = $^x; }', X::Placeholder::Attribute,
    placeholder => '$^x',
    message => /'Cannot use placeholder parameter $^x in an attribute initializer'/;

# Plain initializers and non-placeholder expressions are fine.
lives-ok { EVAL 'class D { has $.a = 5; }' }, 'plain default is allowed';

lives-ok { EVAL 'class E { has $.a = 1 + 2; }' }, 'expression default is allowed';

# A placeholder captured by a nested block initializer is allowed.
lives-ok { EVAL 'class F { has $.a = { $^b }; }' },
    'placeholder inside a block initializer is allowed';
