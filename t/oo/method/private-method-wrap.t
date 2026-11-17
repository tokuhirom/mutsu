use Test;

# A wrapped private method runs its wrapper chain (a trait wrapping `method !m`,
# LibTmux's `is single-operation`).

plan 5;

my @seen;
multi sub trait_mod:<is>(Routine:D $r, :$checked!) {
    $r.wrap(sub (|c) { @seen.push(c.hash.keys.sort.join(',')); callsame });
}

class C {
    method !run(Int :$x = 1) is checked { "run $x" }
    method go { self!run(:x(5), :y(2)) }
    method !plain($v) { "plain $v" }
    method go-plain { self!plain(3) }
}

is C.new.go, 'run 5', 'the wrapped private method still runs';
is @seen.join(';'), 'x,y', 'its wrapper saw the call';

C.^find_private_method('plain').wrap(sub (|c) { 'wrapped ' ~ callsame });
is C.new.go-plain, 'wrapped plain 3', '.^find_private_method(...).wrap applies';
is C.^find_private_method('plain').is-wrapped, True, 'is-wrapped';
is C.new.go, 'run 5', 'other private methods are unaffected';
