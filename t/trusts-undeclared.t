use Test;

plan 5;

# `trusts T` where T is not declared anywhere in the compilation unit (nor a
# built-in type) is X::Undeclared (symbol => T, what => Type).

throws-like 'class RT117859 { trusts Bar }', X::Undeclared,
    'trusts an undeclared type', :symbol<Bar>, :what<Type>;

{
    my $err;
    try { EVAL 'class C { trusts NopeNope }'; CATCH { default { $err = $_ } } }
    is $err.symbol, 'NopeNope', 'the .symbol names the undeclared trusted type';
}

# A trusted type declared elsewhere — even later (forward reference) — is fine.
{
    lives-ok { EVAL 'class A { trusts B }; class B { }' },
        'forward-referenced trusted type is allowed';
    lives-ok { EVAL 'class B { }; class A { trusts B }' },
        'already-declared trusted type is allowed';
}

# Trusting a built-in type is allowed.
{
    lives-ok { EVAL 'class A { trusts Int }' },
        'trusting a built-in type is allowed';
}
