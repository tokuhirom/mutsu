use Test;

# An `is copy` (or `is rw`) parameter must be writable even when an outer
# routine in the call chain has a readonly parameter of the *same* name. The
# readonly set is keyed by bare name and shared across frames, so without
# shadowing the caller's readonly mark leaks in and the inner copy param wrongly
# appears readonly. Regression: integration/advent2013-day06.t.

plan 6;

# sub -> sub, both named $d
sub inner($d is copy) { $d++; $d }
sub outer($d) { inner($d) }
is outer(5), 6, 'is copy sub param writable despite same-named readonly caller param';

# method -> method, both named $d (the day06 shape)
class C {
    method based-on($d is copy) {
        $d++ until $d >= 10;
        $d;
    }
    method top($d) {
        self.based-on($d);
    }
}
is C.new.top(3), 10, 'is copy method param writable despite same-named readonly caller param';

# is rw target with same-named readonly caller param
sub bump($x is rw) { $x += 100 }
sub via($x) { my $y = $x; bump($y); $y }
is via(1), 101, 'is rw param works with same-named readonly caller param';

# caller's readonly param stays readonly after the inner call returns
sub checks($d) {
    inner($d);          # inner's copy param transiently unmarks "d"
    $d = 99;            # must still be readonly here
}
dies-ok { checks(7) }, "caller's readonly param remains readonly after inner copy-param call";

# nested loop mutation (until) on a copy param, day06 idiom
class D {
    has $.step = 7;
    method walk(Int $d is copy where { $_ > 0 }) {
        ++$d until $d %% $.step;
        $d;
    }
    method run(Int $d) { self.walk($d) }
}
is D.new.run(3), 7, 'is copy + where method param mutated in until loop';

# the original value at the caller is unaffected by the copy
sub keep($v) { inner($v); $v }
is keep(42), 42, 'copy param mutation does not write back to caller value';
