use Test;

plan 2;

# A `Proxy` subclass declared in a parametric role body: the STORE closure
# built in each concretization must see that concretization's parameter (#12162).
my @seen;
role R[$method = "push"] {
    my class P is Proxy { }
    method keys() {
        (1,).map: -> $key {
            P.new(FETCH => { $key }, STORE => -> $, $new { @seen.push: $method; $new })
        }
    }
}

my $j = R["append"].new;
$_ = "foo" for $j.keys;
my $i = R.new;
$_ = "foo" for $i.keys;

is @seen[0], "append", "first concretization sees its own parameter";
is @seen[1], "push", "later concretization sees its own parameter";
