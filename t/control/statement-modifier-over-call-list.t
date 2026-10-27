use Test;

plan 5;

# A statement modifier after `f(...), g(...)` governs the whole list, not only
# the last item. The first item is an imported (Test) routine called with
# parentheses, which the statement-level call parser used to cut short.
my @log;
sub note-it($x) { @log.push($x) }

note-it("a"), note-it("b") if 0;
is @log.elems, 0, 'user sub list under a false `if` is skipped entirely';

note-it("c"), note-it("d") with %*ENV<MUTSU_SURELY_NOT_SET_XYZ>;
is @log.elems, 0, 'user sub list under an undefined `with` is skipped entirely';

note-it("e"), note-it("f") if 1;
is @log.join(','), 'e,f', 'both items run under a true `if`';

my $ran = 0;
skip("skipped once"), $ran++ if 0;
is $ran, 0, 'imported paren call, then a comma, then a false `if`: nothing runs';

ok(True, 'first'), ok(True, 'second') unless 1;
pass('an imported paren-call list under a false `unless` ran nothing');
