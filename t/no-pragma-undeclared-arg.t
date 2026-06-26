use Test;

plan 4;

# A bare identifier as a positional argument to `use`/`no` is a term
# reference, not an import symbol (those are strings / `<...>` / `:tags`).
# An undeclared one is X::Undeclared::Symbols, for both `use` and `no`.
# See roast/S32-exceptions/misc.t (old-issue-tracker #3649).
throws-like 'use DoesNotMatter Undeclared;', X::Undeclared::Symbols,
    'use Module BareWord (undeclared) throws X::Undeclared::Symbols';
throws-like 'no DoesNotMatter Undeclared;', X::Undeclared::Symbols,
    'no Module BareWord (undeclared) throws X::Undeclared::Symbols';

# Legal `no` pragmas without a bareword argument must remain unaffected.
{
    my $out = EVAL 'no strict; $undeclared_ok = 42; $undeclared_ok';
    is $out, 42, 'no strict still works (no false undeclared positive)';
}
{
    lives-ok { EVAL 'no worries; 1' }, 'no worries pragma still works';
}
