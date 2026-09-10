use Test;

plan 11;

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

# A placeholder captured by an inner BARE block is fine — the bare block has no
# signature of its own, so the placeholder becomes its parameter. A pointy block
# always has one (even `-> { … }`, which declares zero parameters), so the same
# placeholder there is an override and rakudo rejects it at compile time
# (roast/S04-declarations/implicit-parameter.t test 16).
lives-ok { EVAL 'sub g() { { $^y } }' },
    'placeholder captured by nested bare block is allowed';

throws-like 'sub g() { -> { $^y } }', X::Signature::Placeholder,
    placeholder => '$^y';

# The message is the canonical Rakudo one.
throws-like 'sub h() { $^z }', X::Signature::Placeholder,
    message => /"Placeholder variable '\$^z' cannot override existing signature"/;

# `@_` / `%_` are legal when explicitly declared as parameters — they do not
# override the signature, they ARE the signature, so the declaration must not
# raise X::Signature::Placeholder.
lives-ok { EVAL 'sub f(%_) { %_<a> }' },
    'explicitly declared %_ parameter is allowed';
lives-ok { EVAL 'sub f(@_) { @_[0] }' },
    'explicitly declared @_ parameter is allowed';

# `$^X` (a single uppercase letter after the caret) is a Perl 5 special
# variable, not a placeholder, so it must not raise X::Signature::Placeholder.
lives-ok { EVAL 'sub f($a) { my $s = qq/x$^X/ }' },
    'single-uppercase caret var ($^X) is not a placeholder';
