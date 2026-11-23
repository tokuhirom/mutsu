use Test;
plan 3;

sub add1(Int $x) { return $x + 1 }
is add1(2), 3, 'type-checked positional param';
# Through a variable: a literal `add1("a")` is refuted at CHECK time (#18874).
my $a = "a";
dies-ok { add1($a) }, 'type check fails on Str';

class Holder {
    has $!x;
    method set(Int $x) { $!x = $x; return $x }
}
my $h = Holder.new();
dies-ok { $h.set("x") }, 'type check fails on method';
