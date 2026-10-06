use Test;

# ADR-11276 slice 3A: the guard step in front of the built-in method table.
# A row may declare named arguments, may take any plain argument, and may need
# the interpreter. Each case compares with what Rakudo answers.

plan 5;

subtest 'a named argument bound by a row', {
    plan 5;
    my @nested = 1, [2, [3, 4]];
    is-deeply @nested.flat(:hammer).List, (1, 2, 3, 4).List,
        'flat(:hammer) flattens every level';
    is-deeply (1, (2, (3, 4))).flat(:hammer).List, (1, 2, 3, 4).List,
        'flat(:hammer) on a List';
    is-deeply @nested.flat.List, (1, $[2, [3, 4]]).List,
        'flat itself still flattens one level';
    is-deeply (1, (2, 3)).flat(:foo).List, (1, 2, 3).List,
        'an undeclared named argument is swallowed by the implicit *%_';
    is-deeply (1, (2, 3)).flat(:hammer(False)).List, (1, 2, 3).List,
        'a false :hammer is not the adverb';
}

subtest 'a row that takes any plain argument', {
    plan 6;
    my @a = 1..4;
    is @a.combinations(2).elems, 6, 'combinations(Int)';
    is @a.combinations(1..2).elems, 10, 'combinations(Range)';
    is @a.combinations(1^..3).elems, 10, 'combinations with an exclusive Range';
    is @a.combinations(-1).elems, 0, 'a negative size has no combinations';
    is @a.combinations("2").elems, 6, 'combinations(Str) numifies';
    is (1, 2, 3).combinations(0..*).elems, 8, 'an infinite Range stops at the length';
}

subtest 'a Junction argument still autothreads', {
    plan 2;
    my @a = 1..4;
    my $threaded = @a.combinations(1|2);
    isa-ok $threaded, Junction, 'the call threads over the Junction';
    ok $threaded.raku.starts-with('any('), 'an any() of the two results';
}

subtest 'a row that needs the interpreter', {
    plan 4;
    is-deeply <b a B A>.collate.List, <a A b B>.List, 'collate on a List';
    my @words = <b a B A>;
    is-deeply @words.collate.List, <a A b B>.List, 'collate on an Array';
    is-deeply 5.collate.List, (5,).List, 'collate on an Int is a one-element list';
    {
        my $*COLLATION = Collation.new;
        $*COLLATION.set(:!tertiary, :!quaternary);
        is-deeply <b a B A>.collate.List, <a A b B>.List,
            'collate reads $*COLLATION from the interpreter';
    }
}

subtest 'a user override still wins over a row', {
    plan 2;
    # EVAL: an `augment` in the file body would apply from the start and
    # change the answers above.
    EVAL q[use MONKEY-TYPING; augment class List { method collate() { 'augmented' } }];
    is EVAL(q[<b a>.collate]), 'augmented', 'an augmented List.collate is not bypassed';
    is (1, 2, 3).combinations(2).elems, 3, 'other rows keep answering';
}
