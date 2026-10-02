use Test;

# From ecosystem Date::Calendar::Hebrew: a `my $day` inside a sub called from a
# method of a class declaring `has Int $.day where {...}` is a plain lexical,
# not the attribute, so the attribute's where clause must not apply to it.

plan 3;

class C {
    has Int $.day where { 1 <= $_ <= 30 };
    method BUILD(Int:D :$day) {
        $!day = $day;
        $!day = helper() % 30 + 1;
    }
    sub helper() {
        my $day = 2110374;
        $day = 2110375;
        $day;
    }
    method set-bad() { $!day = 99 }
}

my $c;
lives-ok { $c = C.new(day => 5) }, 'same-named lexical in a helper sub is unconstrained';
dies-ok { $c.set-bad }, 'the attribute where clause still applies to $!day';
is $c.day, 2110375 % 30 + 1, 'attribute unchanged by the rejected store';
