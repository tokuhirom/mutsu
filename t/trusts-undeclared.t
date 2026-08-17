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
# Each subtest below uses its own class names: a plain (non-lexical) `class`
# declared in one EVAL and again in a later, sibling EVAL is a genuine
# X::Redeclaration in raku (both install into the shared GLOBAL package), so
# reusing `A`/`B` across these independent EVALs would make a later subtest
# fail for an unrelated reason (redeclaration, not the trusts check this file
# pins).
{
    lives-ok { EVAL 'class TrustsFwdA { trusts TrustsFwdB }; class TrustsFwdB { }' },
        'forward-referenced trusted type is allowed';
    lives-ok { EVAL 'class TrustsPreB { }; class TrustsPreA { trusts TrustsPreB }' },
        'already-declared trusted type is allowed';
}

# Trusting a built-in type is allowed.
{
    lives-ok { EVAL 'class TrustsBuiltinA { trusts Int }' },
        'trusting a built-in type is allowed';
}
