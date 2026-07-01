use Test;

plan 5;

# A symbolic scalar deref `$::('name') = ...` used in *expression* context
# (e.g. as a list-prefix / expr-listop argument) must be recognised as an
# assignment. Previously the expression parser had no arm for `SymbolicDeref`
# on the LHS of `=`, so the `=` was left unconsumed and a surrounding listop
# like `flat` swallowed the bare deref -- the outer parser then mis-read
# `flat $::('b')` as an assignable call and died with "Unknown call: flat".

{
    our $b;
    my $r = ($::('b') = 5);
    is $r, 5, 'parenthesised symbolic-deref assignment yields the value';
    is $b, 5, '... and writes through to the target';
}

{
    sub l { (1, 2) }
    our $c;
    # `flat` is an expr-listop; its argument is the assignment expression. This
    # used to die at parse/lower time with "Unknown call: flat".
    my @z;
    lives-ok { @z = (flat $::('c') = l(), l()) },
        'flat + symbolic-deref assignment parses (no "Unknown call: flat")';
    ok @z.elems > 0, '... and produces a flattened list';
    ok $c.defined, '... and the target was written through';
}
