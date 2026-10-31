use Test;
use OO::Monitors;

plan 2;

monitor Jar {
    has @!c;
    method add($n) {
        sub same($x, $cs) { $cs eq $x }
        my @set = @!c.grep(-> $cs { same($n, $cs) });
        @!c.push($n) if @set.elems == 0;
        @!c.elems
    }
}

my $j = Jar.new;
is $j.add('a'), 1, 'first add stores the value';
is $j.add('a'), 1, 'a sub nested in a monitor method still matches the stored value';
