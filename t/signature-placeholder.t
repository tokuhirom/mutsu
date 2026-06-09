use Test;

plan 7;

# A routine with an explicit signature (even empty) may not use placeholder
# variables in its body -> X::Signature::Placeholder.
throws-like 'sub f() { $^x }', X::Signature::Placeholder,
    line => 1, placeholder => '$^x';

throws-like 'sub f($a) { $^x }', X::Signature::Placeholder,
    placeholder => '$^x';

throws-like 'sub f() { @_ }', X::Signature::Placeholder,
    placeholder => '@_';

throws-like 'sub f() { $:named }', X::Signature::Placeholder,
    placeholder => '$:named';

# Without an explicit signature, placeholders define the signature implicitly.
lives-ok { EVAL 'sub f { $^x }; f(1)' }, 'placeholder defines implicit signature';

# A placeholder captured by an inner block is fine.
lives-ok { EVAL 'sub g() { -> { $^y } }' },
    'placeholder captured by nested block is allowed';

# The message is the canonical Rakudo one.
throws-like 'sub h() { $^z }', X::Signature::Placeholder,
    message => /"Placeholder variable '\$^z' cannot override existing signature"/;
