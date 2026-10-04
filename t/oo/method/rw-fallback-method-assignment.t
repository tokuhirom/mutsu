use Test;

# From CSS::TagSet (CSS::Properties): `$obj.name = v` where `name` is no method
# and the class has an `is rw` FALLBACK writes through the container it returns.
plan 4;

class C {
    has %!v;
    multi method FALLBACK(Str \name) is rw { %!v{name} }
    method vals { %!v.sort.map(*.kv.join('=')).join(',') }
}

my $c = C.new;
$c.height = 5;
is $c.height, 5, 'read back through FALLBACK';
$c.width = 7;
is $c.vals, 'height=5,width=7', 'both stored in the FALLBACK container';
$c.height = 6;
is $c.height, 6, 'overwrite';

class D { method FALLBACK($n) { 1 } }
dies-ok { D.new.nothing = 3 }, 'a non-rw FALLBACK refuses assignment';
