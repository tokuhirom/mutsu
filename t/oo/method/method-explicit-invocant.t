use Test;

plan 5;

class Counter {
    has $.value;

    method set-to($this: $n) returns Counter {
        $!value = $n;
        $this;
    }

    method add($this: $n) returns Counter {
        $!value += $n;
        $this;
    }
}

my $c = Counter.new(value => 10);
isa-ok $c, Counter, "constructed counter";
is $c.set-to(20).add(5).value, 25, "explicit invocant method chains";
ok $c.set-to(30) === $c, "invocant alias returns same instance";
ok $c.add(2) === $c, "second method also returns same instance";
is $c.value, 32, "chained mutations applied";
