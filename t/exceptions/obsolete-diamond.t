use Test;

# A bare empty `<>` is the obsolete Perl diamond, not an empty word list.
# Raku rejects it at compile time with X::Obsolete (old => "<>").

plan 7;

throws-like '<>', X::Obsolete, 'empty diamond', old => "<>";
throws-like 'my $x = <>', X::Obsolete, 'empty diamond in assignment';

# `< >` (with whitespace) is a legal empty List, NOT the diamond.
isa-ok < >, List, '< > (whitespace) is an empty List';
is < >.elems, 0, '.. and it is really empty';

# Non-empty angle word lists are unaffected.
is-deeply <a b c>, ("a", "b", "c"), 'word list still works';
is <42>, 42, 'single-word allomorph still works';

# Hash zen-slice `%h<>` is a different construct and keeps working.
is-deeply (my %h = a => 1, b => 2)<>.sort, (:a(1), :b(2)),
    'hash zen-slice is not the diamond';
