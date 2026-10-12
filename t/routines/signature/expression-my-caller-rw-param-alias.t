use Test;

# A callee's expression-position `my Str $str` is a fresh binding. A caller's
# same-named typed `is rw` parameter aliases `$str` to its argument's variable;
# that alias must not reach the callee's declaration, nor a later by-name store
# to it (#13007).

plan 12;

my $d = "x";

sub p ($w) { (my Str $str = $w) }
sub go1 (Str $str is rw) { p("y") }
is go1($d), "y", 'first call answers the callee binding';
is $d, "x", 'first call leaves the caller argument alone';
is go1($d), "y", 'second call answers the callee binding';
is $d, "x", 'second call leaves the caller argument alone';

sub p2 ($w) { (my Str $str = $w); $str = "z"; $str }
sub go2 (Str $str is rw) { p2("y") }
go2($d);
is go2($d), "z", 'a later by-name store stays in the callee binding';
is $d, "x", 'a later by-name store does not reach the caller argument';

sub p3 ($w) { if (my Str $str = $w) { $str ~= "!" }; $str }
sub go3 (Str $str is rw) { p3("y") }
go3($d);
is go3($d), "y!", 'an `if (my Str $x = ...)` binding is the callee own';
is $d, "x", 'and does not write the caller argument';

sub go4 (Str $str is rw) { p("y"); $str = "w"; p("y"); $str }
is go4($d), "w", 'the caller rw parameter still writes through after the callee';
is $d, "w", 'the caller argument sees the rw write';
$d = "x";
is go4($d), "w", 'and again on a second call';
is $d, "w", 'the caller argument sees the second rw write';
