use Test;

plan 4;

subtest 'flat rows preserve List and Array semantics', {
    plan 4;
    my $list = ((1, 2), (3, 4));
    my @array = [1, 2], [3, 4];
    is-deeply $list.flat.List, (1, 2, 3, 4).List, 'List.flat descends into nested Lists';
    is-deeply @array.flat.List, ([1, 2], [3, 4]).List,
        'Array.flat preserves itemized child Arrays';
    is-deeply (^3).map({ $list.flat.elems }).List, (4, 4, 4).List,
        'a List.flat call site answers repeatedly';
    is-deeply (^3).map({ @array.flat.elems }).List, (2, 2, 2).List,
        'an Array.flat call site answers repeatedly';
}

subtest 'sort rows keep the native and comparator paths', {
    plan 10;
    my $list = (3, 1, 2);
    my @array = 3, 1, 2;
    my %hash = b => 2, a => 1;
    is-deeply $list.sort.List, (1, 2, 3).List, 'List.sort';
    is-deeply @array.sort.List, (1, 2, 3).List, 'Array inherits the Any.sort row';
    is-deeply %hash.sort.List, (a => 1, b => 2).List, 'Any.sort orders Hash pairs';
    is-deeply 42.sort.List, (42,).List, 'Any.sort wraps a scalar in a one-element Seq';
    is-deeply (3, 1, 2).Seq.sort.List, (1, 2, 3).List,
        'Seq.sort keeps its collection-specific path';
    is-deeply (1, 1, 2, 3, 3, 3).Bag.sort.map(*.gist).List,
        ('1 => 2', '2 => 1', '3 => 3').List, 'Bag.sort decomposes into weighted pairs';
    is-deeply (1..3).sort.List, (1, 2, 3).List, 'Range.sort keeps its collection-specific path';
    is 'ba'.NFC.sort.join(','), '97,98', 'Uni.sort decomposes into codepoints';
    is-deeply @array.sort({ $^b <=> $^a }).List, (3, 2, 1).List,
        'a comparator argument stays on the cascade';
    is-deeply (^3).map({ @array.sort.elems }).List, (3, 3, 3).List,
        'an Array.sort call site answers repeatedly';
}

subtest 'unique and repeated rows cover reified collections', {
    plan 12;
    my $list = (1, 2, 2, 3, 2);
    my @array = 1, 2, 2, 3, 2;
    my %hash = a => 1, b => 2;
    is-deeply $list.unique.List, (1, 2, 3).List, 'List.unique';
    is-deeply $list.repeated.List, (2, 2).List, 'List.repeated';
    is-deeply @array.unique.List, (1, 2, 3).List, 'Array.unique';
    is-deeply @array.repeated.List, (2, 2).List, 'Array.repeated';
    is-deeply (1, 2, 2, 3).Seq.unique.List, (1, 2, 3).List,
        'Seq.unique stays on the shared cascade handler';
    is-deeply (1, 2, 2, 3).Slip.repeated.List, (2).List,
        'Slip.repeated stays on the shared cascade handler';
    is-deeply %hash.unique.sort.List, %hash.sort.List,
        'Any.unique iterates Hash pairs without imposing an order';
    is-deeply %hash.repeated.List, ().List, 'Any.repeated sees no repeated Hash pairs';
    is-deeply (^3).map({ @array.unique.elems }).List, (3, 3, 3).List,
        'an Any.unique call site answers repeatedly';
    is-deeply (^3).map({ @array.repeated.elems }).List, (2, 2, 2).List,
        'an Any.repeated call site answers repeatedly';
    class TransformRowValue {
        has $.n;
        method WHICH { ValueObjAt.new("TransformRowValue|$!n") }
    }
    my @objects = TransformRowValue.new(:n(1)), TransformRowValue.new(:n(1)),
        TransformRowValue.new(:n(2));
    is @objects.unique.elems, 2, 'unique dispatches user-defined WHICH values';
    is @objects.repeated.elems, 1, 'repeated dispatches user-defined WHICH values';
}

subtest 'nonzero arities and scalar behavior keep their fallback', {
    plan 4;
    my @array = 3, 1, 2, 2;
    is 42.unique, 42, 'Any.unique preserves scalar behavior';
    is-deeply 42.repeated.List, ().List, 'Any.repeated on a scalar is empty';
    is-deeply @array.unique(:unknown).List, (3, 1, 2).List,
        'named unique arguments keep the existing cascade behavior';
    is-deeply @array.sort(:unknown).List, (1, 2, 2, 3).List,
        'named sort arguments keep the existing cascade behavior';
}
