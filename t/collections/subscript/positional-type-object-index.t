use Test;

# #9773: a positional subscript indexed by a type object (an uninitialized
# `my $i;`, or a bare type used directly) used to answer `Nil`/`False`
# instead of throwing, and a Callable index answered the sentinel `Nil`
# for an out-of-range result instead of the array's real `Any` default. A
# Failure held in the index was silently dropped instead of propagating the
# exception it wraps. raku's `postcircumfix:<[ ]>` refuses a type-object
# index outright: "Unable to call postcircumfix @a[ (Any) ] with a type
# object / Indexing requires a defined object" (confirmed against real raku
# by the 2026-09-27 doc-diff sweep, `docs/doc-diff-sweep/reports/
# Language__perl-func.txt` and `Language__perl-nutshell.txt`).

plan 11;

throws-like { my @a = 1, 2; my $i; @a[$i] }, Exception,
    message => /'Indexing requires a defined object'/,
    'a bare read with an undefined-scalar index throws';

throws-like { my @a = 1, 2; @a[Int] }, Exception,
    message => /'Indexing requires a defined object'/,
    'a bare read with a literal type-object index throws too';

throws-like { my @a = 1, 2; my $i; @a[$i]:exists }, Exception,
    message => /'Indexing requires a defined object'/,
    ':exists with an undefined-scalar index throws instead of stringifying it into a key';

throws-like { my @a = 1, 2; my $i; @a[$i]:delete }, Exception,
    message => /'Indexing requires a defined object'/,
    ':delete with an undefined-scalar index throws';

{
    # A defined index is unaffected: the throw is specific to a type object.
    my @a = 1, 2;
    is @a[0], 1, 'a defined Int index still reads normally';
    ok @a[0]:exists, 'a defined Int index still answers :exists normally';
}

{
    # A Callable index actually runs, and an out-of-range result reads back
    # as the array's own `Any` default -- not the internal `Nil` sentinel.
    my @e;
    is @e[{0}].^name, 'Any', 'a Callable index into an empty array answers Any';
    is @e[* div 2].^name, 'Any', 'a WhateverCode index answers Any too';

    my @a = 1, 2;
    is @a[{5}].^name, 'Any', 'a Callable index past the end answers Any, not Nil';
}

{
    # A Failure held in the index propagates the exception it wraps instead
    # of being silently swallowed.
    my @e;
    throws-like { @e[1 div 0] }, X::Numeric::DivideByZero,
        'a Failure index (divide-by-zero) propagates its wrapped exception';
}

{
    # A hash target keeps its own (unrelated) undefined-key behavior: it
    # stringifies with a warning rather than throwing "type object".
    my %h;
    my $k;
    my $got = %h{$k}:exists;
    ok !$got, 'an undefined key on a hash still just answers False, not a throw';
}
