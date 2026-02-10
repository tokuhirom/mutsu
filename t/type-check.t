use Test;
plan 3;

sub add1(Int $x) { return $x + 1 }
is add1(2), 3, 'type-checked positional param';
dies-ok { add1("a") }, 'type check fails on Str';

class Holder {
    method set(Int $x) { $!x = $x; return $x }
}
my $h = Holder.new();
dies-ok { $h.set("x") }, 'type check fails on method';
