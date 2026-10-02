use Test;

# A loop label in term position is a first-class `Label` object, and loop
# control accepts it dynamically: `next |c`, `next(LABEL)`, `LABEL.next`
# (GH #10737).
plan 21;

{
    my sub f(|c) { next |c }
    my @seen;
    L1: for 1..2 -> $i { for 1..3 -> $j { f(L1) if $j == 2; @seen.push("$i$j") } }
    is @seen, <11 21>, 'next |c with a Label capture continues the labelled loop';
}

{
    my sub g(|c) { last |c }
    my @seen;
    L2: for 1..3 -> $i { for 1..3 -> $j { g(L2) if $i == 2; @seen.push("$i$j") } }
    is @seen, <11 12 13>, 'last |c with a Label capture leaves the labelled loop';
}

{
    my @seen;
    L3: for 1..2 -> $i { for 1..3 -> $j { next(L3) if $j == 2; @seen.push("$i$j") } }
    is @seen, <11 21>, 'next(LABEL) is the routine form';
}

{
    my @seen;
    L4: for 1..2 -> $i { for 1..3 -> $j { L4.next if $j == 2; @seen.push("$i$j") } }
    is @seen, <11 21>, 'LABEL.next';
}

{
    my sub h($l) { $l.last }
    my $reached = False;
    L5: for 1..3 { for 1..3 { h(L5) }; $reached = True }
    nok $reached, 'Label.last from a called routine leaves the labelled loop';
}

{
    my $c = 0;
    my @seen;
    L6: for 1..2 { $c++; L6.redo if $c == 1; @seen.push($_) }
    is $c, 3, 'Label.redo re-runs the iteration';
    is @seen, [1, 2], 'Label.redo keeps the topic';
}

{
    L7: while True { L7.last }
    pass 'Label.last leaves a labelled while loop';
}

FOO: for 1 {
    is FOO.^name, 'Label', 'a label term is a Label';
    isa-ok FOO, Label, 'smartmatches Label';
    is FOO.name, 'FOO', '.name';
    is FOO.line, $?LINE - 4, '.line is the declaration line';
    is FOO.file.IO.basename, 'label-first-class.t', '.file is the declaring file';
    is FOO.Str, "FOO {FOO.file}:{FOO.line}", '.Str';
    is FOO.raku, "Label.new(name => \"FOO\", file => \"{FOO.file}\", line => {FOO.line})", '.raku';
    like FOO.gist, /^ 'Label<FOO>(at ' .* '<HERE>FOO: for 1 {' /, '.gist quotes the declaration';
    my $x = FOO;
    ok $x === FOO, 'every reference is the same object';
}

{
    my $l;
    BAR: for 1..3 { $l = BAR; last }
    throws-like { $l.next }, X::ControlFlow, illegal => 'labeled next',
        'Label.next outside its loop is X::ControlFlow';
}

{
    my sub n(|c) { next |c }
    throws-like { for 1 { n(42) } }, Exception,
        message => /'Cannot resolve caller next(Int:D)'/,
        'next with a non-Label argument has no candidate';
}

{
    my-label: for 1..2 {
        is my-label.name, 'my-label', 'a lowercase hyphenated label is a Label too';
        last my-label;
    }
}

{
    my %h;
    PAIR: for 1 { %h = (PAIR => 1) }
    is %h.keys, ('PAIR',), 'LABEL => ... is still a pair with a string key';
}
