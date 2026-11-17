use v6;
use Test;

# The constructor lane (`src/vm/vm_ctor_lane.rs`) also serves a class with a
# BUILD or TWEAK submethod: once one `.new` has walked the dispatch chain into
# the native default constructor, later calls replay that constructor, BUILD
# and TWEAK included. Every construction runs in a loop so the later
# iterations take the lane; each must answer what the full dispatch answers.

plan 8;

{
    my $built = 0;
    my $tweaked = 0;
    class Counted {
        has $.id;
        has $.n = 3;
        submethod BUILD(:$id) { $!id = $id; $built++ }
        submethod TWEAK { $tweaked++ }
    }
    my @made = (^5).map({ Counted.new(id => $_) });
    is @made.map(*.id).join(','), '0,1,2,3,4', 'BUILD binds the named argument on every call';
    is @made.map(*.n).join(','), '3,3,3,3,3', 'the initializer applies after BUILD on every call';
    is "$built $tweaked", '5 5', 'BUILD and TWEAK write a captured outer lexical on every call';
}

{
    my $seed = 0;
    class Reads {
        has $.v;
        submethod BUILD { $!v = $seed }
    }
    my @vals;
    for ^3 -> $i { $seed = $i * 7; @vals.push: Reads.new.v }
    is @vals.join(','), '0,7,14', 'BUILD reads the current value of an outer lexical';
}

{
    class Fails {
        has $.x;
        submethod BUILD(:$x) { $!x = $x; fail "bad x" if $x < 0 }
    }
    my @r = (1, -1, 2).map({ Fails.new(x => $_) });
    is @r.map({ $_ ~~ Failure ?? 'F' !! .x }).join(','), '1,F,2',
        'a fail in BUILD yields a Failure on a replayed call too';
}

{
    class Required {
        has $.r is required;
        submethod BUILD(:$r) { $!r = $r if $r.defined }
    }
    my @ok = (^3).map({ Required.new(r => $_).r });
    is @ok.join(','), '0,1,2', 'a required attribute set by BUILD';
    throws-like { Required.new }, X::Attribute::Required,
        'an unset required attribute still dies after the lane is warm';
}

{
    class Grows {
        has $.a;
        submethod BUILD(:$a) { $!a = $a }
    }
    my @before = (^3).map({ Grows.new(a => $_).a });
    Grows.^add_method('twice', method { $.a * 2 });
    my @after = (^3).map({ Grows.new(a => $_).twice });
    is (|@before, |@after).join(','), '0,1,2,0,2,4',
        'a class changed by the MOP keeps constructing through BUILD';
}
