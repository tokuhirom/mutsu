use Test;

plan 4;

# `next` inside a whenever body skips the rest of the body for THIS value
# only (Rakudo absorbs the control exception in the supply machinery). It
# must not surface as a supply failure / quit. Cro::HTTP::ResponseSerializer
# uses `next` to short-circuit bodyless (204) responses.

# Supplier-backed source (live emit dispatch).
my $in1 = Supplier.new;
my @got1;
my $quit1 = False;
(supply {
    whenever $in1.Supply -> $v {
        if $v == 1 { emit "one"; next; }
        emit "other:$v";
    }
}).tap: -> $x { @got1.push($x) }, quit => -> $ex { $quit1 = True };
$in1.emit(1);
$in1.emit(2);
is @got1.join(","), "one,other:2", 'next in whenever body (live source) skips per value';
nok $quit1, 'no quit fired for the live source';

# Chained on-demand source (the supply drive loop path).
my @got2;
my $quit2 = False;
(supply {
    whenever (supply { emit 1; emit 2; }) -> $v {
        if $v == 1 { emit "one"; next; }
        emit "other:$v";
    }
}).tap: -> $x { @got2.push($x) }, quit => -> $ex { $quit2 = True };
is @got2.join(","), "one,other:2", 'next in whenever body (on-demand source) skips per value';
nok $quit2, 'no quit fired for the on-demand source';
