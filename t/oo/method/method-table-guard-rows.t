use Test;

# ADR-11276 slice 3A: the guard step in front of the built-in method table.
# A row may declare named arguments, may take any plain argument, and may need
# the interpreter. Each case compares with what Rakudo answers.

plan 11;

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

subtest 'Bool and Version rows', {
    plan 8;
    is True.key, 'True', 'Bool.key';
    is False.key, 'False', 'Bool.key of False';
    is True.value, 1, 'Bool.value';
    is False.value, 0, 'Bool.value of False';
    my $v = Version.new('1.2.3');
    is-deeply $v.parts, (1, 2, 3).List, 'Version.parts';
    is Version.new('1.2.*').parts.elems, 3, 'Version.parts with a whatever';
    ok Version.new('1.2+').plus, 'Version.plus';
    ok Version.new('1.*').whatever && !$v.whatever, 'Version.whatever';
}

subtest 'Range and Pair rows', {
    plan 8;
    is-deeply (1..3).excludes-min, False, 'Range.excludes-min';
    is-deeply (1^..3).excludes-min, True, 'Range.excludes-min on 1^..3';
    is-deeply (1..^3).excludes-max, True, 'Range.excludes-max';
    is-deeply ('a'..'c').excludes-max, False, 'Range.excludes-max on a string range';
    my $p = a => 1;
    is $p.key, 'a', 'Pair.key';
    is $p.value, 1, 'Pair.value';
    is-deeply $p.antipair, 1 => 'a', 'Pair.antipair';
    my $data = Pair.new(2, 'two');
    is-deeply ($data.key, $data.value, $data.antipair), (2, 'two', 'two' => 2),
        'a data Pair answers the same rows';
}

subtest '.Bool on the collection owners', {
    plan 14;
    is-deeply set().Bool, False, 'an empty Set';
    is-deeply set(1).Bool, True, 'a Set with an element';
    is-deeply SetHash.new.Bool, False, 'an empty SetHash';
    is-deeply bag(1).Bool, True, 'a Bag with an element';
    is-deeply BagHash.new.Bool, False, 'an empty BagHash';
    is-deeply mix(1).Bool, True, 'a Mix with an element';
    is-deeply MixHash.new.Bool, False, 'an empty MixHash';
    is-deeply \().Bool, False, 'an empty Capture';
    is-deeply \(1).Bool, True, 'a Capture with an argument';
    is-deeply \(:a).Bool, True, 'a Capture with a named argument';
    is-deeply (1..2).Bool, True, 'a Range';
    is-deeply 'ab'.NFC.Bool, True, 'a Uni with codepoints';
    is-deeply ''.NFC.Bool, False, 'an empty Uni';
    is-deeply True.Bool, True, 'Bool.Bool';
}

subtest 'Date and DateTime rows', {
    plan 8;
    my $date = Date.new(2024, 3, 5);
    is $date.year, 2024, 'Date.year';
    is $date.month, 3, 'Date.month';
    is $date.day, 5, 'Date.day';
    my $dt = DateTime.new(2024, 3, 5, 7, 8, 9);
    is ($dt.year, $dt.month, $dt.day), (2024, 3, 5), 'DateTime year, month, day';
    is ($dt.hour, $dt.minute), (7, 8), 'DateTime hour and minute';
    class MyDate is Date {}
    my $sub = MyDate.new(2025, 1, 2);
    is ($sub.year, $sub.month, $sub.day), (2025, 1, 2),
        'a subclass of Date has no shape and still answers';
    class MyDateTime is DateTime {}
    my $subdt = MyDateTime.new(2025, 1, 2, 3, 4, 5);
    is ($subdt.hour, $subdt.minute), (3, 4), 'a subclass of DateTime too';
    is Date.new(2024, 3, 5).day-of-week, 2, 'a method with no row still answers';
}

subtest 'type objects answer only the rows that say so', {
    plan 8;
    is-deeply Int.Bool, False, 'Int.Bool';
    is-deeply Num.Bool, False, 'Num.Bool';
    is-deeply Rat.Bool, False, 'Rat.Bool';
    is-deeply FatRat.Bool, False, 'FatRat.Bool';
    is-deeply Complex.Bool, False, 'Complex.Bool';
    my $t = Int;
    is-deeply $t.Bool, False, 'a type object held in a variable';
    class Plain { }
    is-deeply Plain.Bool, False, 'a user class type object is not a row receiver';
    throws-like { Int.abs }, Exception, 'Int.abs is not answered on a type object';
}

subtest 'methods still answered by the cascades on the new shapes', {
    plan 8;
    is (1..3).elems, 3, 'Range.elems';
    is set(1, 2).elems, 2, 'Set.elems';
    is-deeply (a => 1).kv.List, ('a', 1).List, 'Pair.kv';
    is-deeply True.pred, False, 'Bool.pred';
    is Version.new('1.2').Str, '1.2', 'Version.Str';
    is \(1, 2).elems, 2, 'Capture.elems';
    is Date.new(2024, 3, 5).gist, '2024-03-05', 'Date.gist';
    is DateTime.new(2024, 3, 5, 7, 8, 9).Str, '2024-03-05T07:08:09Z', 'DateTime.Str';
}
