use Test;

# A bare block in TAIL position of a routine/closure body still runs its
# ENTER/LEAVE phasers. The compiler inlines a tail block to make it the body's
# implicit return value, and inlining drops phasers -- so
# `sub f { ...; { ...; LEAVE cleanup() } }` never cleaned up. The mainline
# (`compiler/mod.rs`) already routed such a block through a real `BlockScope`;
# the routine- and closure-body compilers did not.
#
# Found via Cro::HTTP's `t/http-middleware.rakutest`, where each subtest ends in
# `{ my $service = …; $service.start; …; LEAVE $service.stop() }`: the server was
# never stopped, so the next subtest's requests were answered by the previous
# subtest's still-listening pipeline. See
# https://github.com/tokuhirom/mutsu/issues/7555.

plan 7;

sub run-block(&body) { body() }

{
    my @fired;
    run-block {
        { LEAVE @fired.push('a'); }
        { LEAVE @fired.push('b'); }
    }
    is @fired.join(','), 'a,b',
        'both LEAVEs fire, including the tail block of a block argument';
}

{
    my @fired;
    sub named() {
        { LEAVE @fired.push('p'); }
        { LEAVE @fired.push('q'); }
    }
    named();
    is @fired.join(','), 'p,q', 'tail block of a named sub runs its LEAVE';
}

{
    my @fired;
    my $closure = { { LEAVE @fired.push('c'); } };
    $closure();
    is @fired.join(','), 'c', 'tail block of an anonymous closure runs its LEAVE';
}

{
    my @fired;
    my class Cls { method m(@f) { { LEAVE @f.push('m'); } } }
    Cls.m(@fired);
    is @fired.join(','), 'm', 'tail block of a method runs its LEAVE';
}

{
    my @fired;
    run-block {
        { ENTER @fired.push('enter'); LEAVE @fired.push('leave'); }
    }
    is @fired.join(','), 'enter,leave',
        'ENTER and LEAVE of a tail block both fire, in order';
}

# The tail block is still the body's value.
is run-block({ { 42 } }), 42,
    'a tail block with no phaser still supplies the body value';

{
    my @fired;
    is run-block({ { LEAVE @fired.push('z'); 42 } }), 42,
        'a tail block with a LEAVE still supplies the body value';
}
