use v6;
use Test;

# `Label`'s methods are rows of the method table (ADR-11276, slice 5): they
# answer from the instance's attributes, and next/last/redo raise the labelled
# loop-control signal.

plan 12;

my $seen;
FOO: for 1..3 -> $i {
    $seen = FOO;
    last;
}

isa-ok $seen, Label, 'a loop label term is a Label';
is $seen.name, 'FOO', 'name';
is $seen.file.chars > 0, True, 'file names the source';
is $seen.line.WHAT.gist, '(Int)', 'line is an Int';
like $seen.Str, /^ 'FOO ' .* ':' \d+ $/, 'Str is NAME FILE:LINE';
like $seen.gist, /'FOO'/, 'gist quotes the label';
like $seen.raku, /^ 'Label.new(name => "FOO", file => "' .* '", line => ' \d+ ')' $/, 'raku';

my @out;
LOOPA: for 1..3 -> $i {
    for 1..3 -> $j {
        LOOPA.next if $j == 2;
        @out.push("$i$j");
    }
}
is @out.join(','), '11,21,31', 'Label.next continues the labelled loop';

@out = ();
LOOPB: for 1..3 -> $i {
    for 1..3 -> $j {
        LOOPB.last if $i == 2;
        @out.push("$i$j");
    }
}
is @out.join(','), '11,12,13', 'Label.last leaves the labelled loop';

my $tries = 0;
LOOPC: for 1 {
    $tries++;
    LOOPC.redo if $tries < 3;
}
is $tries, 3, 'Label.redo restarts the iteration';

is $seen.^can('name').so, True, '.^can sees a row';
is $seen.WHAT.gist, '(Label)', 'WHAT';
