use v6;
use Test;

# An inline `subset` (anonymous `::` or named) used as a TERM: an expression
# that yields the subset's type object, usable on the RHS of `~~`, bound to a
# variable, or nested in a `where` clause. Previously each form died with a
# `Confused` parse error.

plan 12;

# Anonymous inline subset on the RHS of a smartmatch.
is (42  ~~ (subset :: where Int|Str)), True,  'anon subset term: Int matches';
is (4.2 ~~ (subset :: where Int|Str)), False, 'anon subset term: Rat fails';
is ("x" ~~ (subset :: where Int|Str)), True,  'anon subset term: Str matches';

# Bound to a variable and reused.
{
    my $s = (subset :: where Int|Str);
    is (42  ~~ $s), True,  'bound anon subset: Int';
    is (4.2 ~~ $s), False, 'bound anon subset: Rat';
}

# `of Base` with a `where` predicate.
is (5 ~~ (subset :: of Real where * > 3)),  True,  'of Real where * > 3: 5';
is (2 ~~ (subset :: of Real where * > 3)),  False, 'of Real where * > 3: 2';

# `of` alone (no predicate).
is (5   ~~ (subset :: of Int)), True,  'of Int only: Int';
is (5.0 ~~ (subset :: of Int)), False, 'of Int only: Rat';

# A named inline subset registers its name for later use.
{
    my $s = (subset InlineNamed where Int);
    is (7 ~~ InlineNamed), True, 'named inline subset usable by name';
}

# Nested inside a signature `where` clause (the doc example shape).
{
    my enum E1 <A B>;
    my enum E2 <C D>;
    sub g(@a where { .all ~~ subset :: where E1|E2 }) { @a.elems }
    is g([A, C]), 2, 'inline subset inside a signature where clause';
}

# `.grep` with an inline subset predicate.
{
    my @vals = 1, "x", 4.2, 3;
    my $s = subset :: where Int|Str;
    is @vals.grep(* ~~ $s).elems, 3, 'inline subset in a grep predicate';
}

done-testing;
