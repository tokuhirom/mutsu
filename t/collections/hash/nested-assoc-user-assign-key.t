use Test;

plan 4;

# Chained subscript assignment where BOTH levels are user-defined Associative
# instances: `$o<support><source> = v` must route the outer read through
# AT-KEY and the innermost write through the inner instance's ASSIGN-KEY
# (META6's AutoAssoc — t/050-assoc.t "Support is writable").
role AA does Associative {
    method AT-KEY($key) {
        my $attr = self.^attributes(:local).grep({ .name eq '$!' ~ $key })[0];
        $attr.defined ?? $attr.get_value(self) !! Nil;
    }
    method ASSIGN-KEY($key, \value) {
        my $attr = self.^attributes(:local).grep({ .name eq '$!' ~ $key })[0];
        $attr.set_value(self, value) if $attr.defined;
    }
}
class Inner does AA {
    has Str $.source is rw;
}
class Outer does AA {
    has Inner $.support is rw;
}
my $o = Outer.new(support => Inner.new(source => "orig"));
is $o<support><source>, "orig", 'chained read through two AT-KEY levels';
$o<support><source> = 'spicy';
is $o<support><source>, 'spicy', 'chained write reaches the inner ASSIGN-KEY';
is $o.support.source, 'spicy', 'the attribute itself was mutated';

# Positional inner level: ASSIGN-POS on a user Positional instance.
role AP does Positional {
    method AT-POS($i) {
        my $attr = self.^attributes(:local)[0];
        $attr.get_value(self)[$i];
    }
    method ASSIGN-POS($i, \value) {
        my $attr = self.^attributes(:local)[0];
        $attr.get_value(self)[$i] = value;
    }
}
class Seat does AP {
    has @.rows;
}
class Hall does AA {
    has Seat $.seats is rw;
}
my $h = Hall.new(seats => Seat.new(rows => ["a", "b"]));
$h<seats>[1] = "z";
is $h.seats.rows[1], "z", 'chained write reaches the inner ASSIGN-POS';

done-testing;
