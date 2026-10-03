use v6;
use Test;

plan 12;

# A `$` variable is a Scalar container whatever it holds, so an `is rw`
# parameter aliases that container even when it holds a Hash, an Array, a
# List or an object (#11077).

{
    sub w($p is rw) { $p = 5 }
    my $h = {};
    w($h);
    is $h, 5, 'is rw param replaces a Hash held in the caller scalar';
    my $a = [1];
    w($a);
    is $a, 5, '... an Array';
    my $l = (1, 2);
    w($l);
    is $l, 5, '... a List';
}

{
    sub w($p is rw) { $p.push(3) }
    my $a = [1];
    w($a);
    is $a, [1, 3], 'mutating the aggregate through the param still works';
    is $a.VAR.^name, 'Scalar', '... and the caller variable stays a Scalar';
}

{
    sub w($p is rw) { return-rw $p }
    my $h = {};
    my $x := w($h);
    $x = 1;
    is $h, 1, 'return-rw of the param hands back the caller container';

    my $r = {};
    w($r) = 1;
    is $r, 1, 'assigning to the call result writes the caller Hash scalar';
    my $s = [];
    w($s) = 2;
    is $s, 2, '... and the caller Array scalar';
    my $i = 0;
    w($i) = 3;
    is $i, 3, '... and still a plain Int scalar';
}

{
    sub w($p is rw) is rw { $p }
    my $h = {};
    w($h) = 4;
    is $h, 4, 'is rw routine tail of an rw param holding a Hash';
}

{
    # TOML::Thumb's walk-key: descend by rebinding the rw param, then assign
    # through the returned container.
    sub walk-key($ptr is rw, @k) {
        for @k { $ptr := $ptr{$_} }
        return-rw $ptr;
    }
    my $root = {};
    walk-key($root, <a b>) = 1;
    is-deeply $root, ${ a => ${ b => 1 } }, 'walk-key assigns through the descended path';
    walk-key($root, <a c>) = 2;
    is-deeply $root, ${ a => ${ b => 1, c => 2 } }, '... and a second key lands beside it';
}
