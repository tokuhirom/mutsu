use Test;

# A callee's `EVAL` must not call a `my sub` of its CALLER, also when the caller
# declares a lexical type beside it (a `my enum`/`my subset`/`my class` made
# the routine ineligible for the frame-lexical binding of ADR-0113, which left
# the sub in the flat routine registry, #16099).

plan 8;

sub callee { (try EVAL q[helper(5)]) // 'not visible' }

sub with-enum   { my enum E <A B>; my sub helper($x) { 1 }; callee() }
sub with-subset { my subset Small of Int where * < 10; my sub helper($x) { 1 }; callee() }
sub with-class  { my class K { }; my sub helper($x) { 1 }; callee() }
sub with-role   { my role R { }; my sub helper($x) { 1 }; callee() }
sub plain       { my sub helper($x) { 1 }; callee() }

is with-enum(),   'not visible', 'a my sub beside a my enum';
is with-subset(), 'not visible', 'a my sub beside a my subset';
is with-class(),  'not visible', 'a my sub beside a my class';
is with-role(),   'not visible', 'a my sub beside a my role';
is plain(),       'not visible', 'a my sub alone (unchanged)';

# The declaring routine still calls its own sub, which may use the type.
sub own { my subset Small of Int where * < 10; my sub ok($x) { 3 }; ok(1) }
is own(), 3, 'the declaring routine calls its own my sub beside a subset';

sub own-typed {
    my subset Small of Int where * < 10;
    my sub chk(Small $x) { $x * 2 }
    chk(4)
}
is own-typed(), 8, 'a my sub taking the routine\'s own subset still works';

sub own-enum { my enum Dir <Up Down>; my sub flip($d) { $d == Up ?? Down !! Up }; flip(Up).Int }
is own-enum(), 1, 'a my sub using the routine\'s enum keys still works';
