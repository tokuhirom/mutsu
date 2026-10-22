use Test;

# A `whenever` nested in the body of a `whenever` over a static source
# (`Supply.from-list`) inside `Promise(supply { ... })` is tapped, not run as a
# plain statement whose registration marker becomes the supply's value
# (#14453, the Cro::HTTP redirect shape of #7546).

plan 5;

my $inner = start { 11 };
is Promise(supply { whenever Supply.from-list(1) -> $x { whenever $inner { emit "a>$_"; done } } }).result,
    "a>11", 'nested whenever over a Promise';

is Promise(supply { whenever Supply.from-list(1) -> $x { whenever Supply.from-list(5) { emit "b>$_"; done } } }).result,
    "b>5", 'nested whenever over a static supply';

my $slow = start { sleep 0.2; 12 };
is Promise(supply { whenever Supply.from-list(1) -> $x { whenever $slow { emit "s>$_"; done } } }).result,
    "s>12", 'nested whenever over a promise that is not yet kept';

is Promise(supply { whenever Supply.from-list(1) { emit "x"; whenever $inner { emit "a>$_"; done } } }).result,
    "a>11", 'a value emitted before the nested whenever is superseded by its emit';

is Promise(supply { whenever Supply.from-list(1) -> $x { emit 6; done } }).result, 6,
    'the top-level form still works';
