use Test;

plan 6;

# A class that declares `AT-POS` itself answers `$obj.AT-POS($key) = $v`
# with any index: `Any`'s `AT-POS` must not coerce a non-Int index with
# `.Int` first (#18782, Red's `Red::Column` index).

class Idx { has $.name }

my @log;

class Store {
    method AT-POS($key) is rw {
        Proxy.new(
            FETCH => -> $ { "fetch " ~ $key.name },
            STORE => -> $, $v { @log.push("store " ~ $key.name ~ "=" ~ $v) },
        )
    }
}

my $s = Store.new;
$s.AT-POS(Idx.new(:name<a>)) = 1;
is @log.tail, 'store a=1', 'method-call lvalue with an object index';
is $s.AT-POS(Idx.new(:name<b>)), 'fetch b', 'rvalue call with an object index';

role CM {
    method AT-POS(\SELF: $key) is rw {
        Proxy.new(
            FETCH => -> $ { "role " ~ $key.name },
            STORE => -> $, $v { @log.push("role store " ~ $key.name ~ "=" ~ $v) },
        )
    }
    method ASSIGN-POS(\SELF: $index, $value) {
        SELF.AT-POS($index) = $value
    }
}

class Composed does CM { }
my $c = Composed.new;
$c.AT-POS(Idx.new(:name<c>)) = 2;
is @log.tail, 'role store c=2', 'a role-composed AT-POS with a \SELF invocant';
$c.ASSIGN-POS(Idx.new(:name<d>), 3);
is @log.tail, 'role store d=3', 'ASSIGN-POS assigning through SELF.AT-POS';

my $m = Store.new but CM;
$m.AT-POS(Idx.new(:name<e>)) = 4;
is @log.tail, 'role store e=4', 'a `but`-mixed AT-POS';

class Plain { }
throws-like { Plain.new.AT-POS(Idx.new) = 5 }, Exception,
    'a class without its own AT-POS still goes through Any';
