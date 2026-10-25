use Test;

# An `is rw` parameter bound to a caller variable that is spelled like ANOTHER
# parameter of the callee (`f("x", $a)` against `f(Str $a, Int $b is rw)`) must not
# replace that parameter with the caller's value: the callee's `$a` is "x".

plan 11;

sub plain(Str $a, Int $b is rw) { "$a|$b" }
my $a = 42;
is plain("x", $a), "x|42", "plain sub, rw param, caller variable named like the other param";

multi mrw(Str $a, Numeric() $b is rw) { "rw $a|$b" }
multi mrw(Str $a, Numeric() $b is readonly) { "ro $a|$b" }
is mrw("x", $a), "rw x|42", "multi with a coercing rw param";
is mrw("x", 42), "ro x|42", "the readonly candidate for a literal";

sub bump(Str $a, Int $b is rw) { $b++; $a }
is bump("y", $a), "y", "the callee's own parameter survives a write to the rw one";
is $a, 43, "the write reaches the caller's variable";

sub swap(Str $a, Str $b is rw) { $b = $a ~ "!"; }
my $b = "keep";
swap("z", $b);
is $b, "z!", "a write to the rw parameter reaches the caller variable";

sub both($x is rw, $y is rw) { $x = "X"; $y = "Y"; }
my ($x, $y) = 1, 2;
both($x, $y);
is "$x$y", "XY", "two rw parameters with the same-named sources";

sub rev($x is rw, $y is rw) { ($x, $y) = ($y, $x); }
my ($p, $q) = "p", "q";
rev($p, $q);
is "$p$q", "qp", "rw swap through parameters";

sub crossed($b is rw, $a is rw) { $a = "from-a"; $b = "from-b"; }
my ($a2, $b2) = <1 2>;
crossed($a2, $b2);
is "$a2 $b2", "from-b from-a", "crossed names: each caller variable gets its parameter's value";

sub nested(Str $a, Int $b is rw) { plain($a, $b) }
is nested("n", $a), "n|43", "nested call";

class K { method m(Str $a, Int $b is rw) { "$a|$b" } }
is K.new.m("k", $a), "k|43", "method with the same shape";
