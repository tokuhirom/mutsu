use Test;

plan 5;

# `emit` is caught by the innermost *dynamically* enclosing supply, not the
# lexically enclosing one. A sub declared outside a supply block still emits
# into it when called from inside.
sub outer-emit($x) { emit $x * 10 }

sub collect($supply) {
    my @got;
    $supply.tap(-> $v { @got.push($v) }, done => { @got.push('done') });
    @got.join(',')
}

is collect(supply { emit 1; emit 2 }), '1,2,done',
    'emit written directly in the body';

is collect(supply { sub e($x) { emit $x }; e(1); emit 2 }), '1,2,done',
    'emit inside a sub declared in the body';

is collect(supply { outer-emit(1); emit 2 }), '10,2,done',
    'emit inside a sub declared outside the block';

is collect(supply { whenever Supply.from-list(1, 2) { outer-emit($_) } }), '10,20,done',
    'emit from a sub called by a whenever over a static supply';

# The live-Supplier path dispatches the whenever body as a tap callback, which
# is where Cro::HTTP2::GeneralParser's `emit-response` helper runs.
my $s = Supplier.new;
my @got;
supply {
    whenever $s.Supply { outer-emit($_) }
}.tap(-> $v { @got.push($v) }, done => { @got.push('done') });
$s.emit(1);
$s.emit(2);
$s.done;
is @got.join(','), '10,20,done',
    'emit from a sub called by a whenever over a live Supplier';
