use Test;

# A Proxy a block hands back is FETCHed when the caller sinks it, as Rakudo
# decontainerizes it: `dies-ok { $obj.rw-accessor }` sees the FETCH die. The
# block's Proxy used to come back untouched and never ran its FETCH (PDF's
# `t/cos-tie-index.t`, `dies-ok {$array.I3}`). Expected values are the `raku`
# (v2026.09) output.

plan 5;

my $fetched = 0;
class C {
    method acc() is rw { Proxy.new(FETCH => -> $ { $fetched++; die "boom" }, STORE => -> $, \n { }) }
}
my $c = C.new;

dies-ok { $c.acc }, 'a block returning a Proxy dies when sunk by dies-ok';
is $fetched, 1, '... after exactly one FETCH';
my $b = { $c.acc };
dies-ok { $b() }, 'a block call returning a Proxy is FETCHed when sunk';
lives-ok { my $y := $c.acc; 1 }, 'binding the Proxy does not FETCH';
is $fetched, 2, '... so only the two sunk calls fetched';
