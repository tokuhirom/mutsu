use Test;

# #12384: a pointy block nested in a map/gather block captures the enclosing
# routine's `&func` parameter, not the same-named parameter of the callee it
# is handed to.

plan 7;

sub inner(&func) { func 5 }

sub via-gather(&func) { gather { take inner(-> $n { func $n }) } }
is-deeply via-gather({ $_ + 1 }).list, (6,), 'pointy in a gather block, forced later';

sub via-gather-eager(&func) {
    my @r = gather { take inner(-> $n { func $n }) };
    @r
}
is-deeply via-gather-eager({ $_ + 1 }), [6], 'pointy in a gather block, forced in the routine';

sub via-map(&func) { (1,).map({ inner(-> $n { func $n }) }) }
is-deeply via-map({ $_ + 1 }).list, (6,), 'pointy in a map block';

sub via-map-pointy(&func) { (1,).map(-> $z { inner(-> $n { func $n }) }) }
is-deeply via-map-pointy({ $_ + 1 }).list, (6,), 'pointy in a pointy map callback';

sub via-map-call(&func) { (1,).map({ inner(-> $n { &func($n) }) }) }
is-deeply via-map-call({ $_ + 1 }).list, (6,), '&func($n) form in a map block';

sub via-map-decl(&func) { (1,).map({ my $x = 1; inner(-> $n { func $n }) }) }
is-deeply via-map-decl({ $_ + 1 }).list, (6,), 'map block that declares a lexical';

sub direct(&func) { inner(-> $n { func $n }) }
is direct({ $_ + 1 }), 6, 'pointy directly in the routine body';
