use Test;

# A sigilless parameter bound to a variable promotes the CALLER's variable to a
# shared cell. The caller's own frame decides whether that cell exists: a
# same-named cell of an OLDER frame (a grand-caller's `$y`) is another variable
# and must not become the argument (Timezones::ZoneInfo's `timesub`, #16177).

plan 6;

sub isleap(\year) { year % 4 == 0 }

sub inner {
    my $y = 1570;
    my $r = isleap($y);
    "$y";
}
sub outer {
    my $y = 120;
    my $a = isleap($y);
    my $in = inner();
    "$y/$in";
}
is outer(), '120/1570', 'the callee keeps its own $y after a same-named grand-caller call';

# Three frames, the middle one called through a code object.
sub middle(&f) {
    my int32 $y = 7;
    my $r = isleap($y);
    f() ~ "/$y";
}
sub top {
    my int32 $y = 3;
    my $r = isleap($y);
    middle(&inner) ~ "/$y";
}
is top(), '1570/7/3', 'every frame keeps its own $y';

# The sigilless parameter still aliases the caller's variable when it IS in
# the caller's frame.
sub bump(\n) is rw { n = n + 1 }
sub calls-bump { my $n = 10; bump($n); $n }
is calls-bump(), 11, 'a sigilless parameter writes through to the caller variable';

sub nested-bump {
    my $n = 100;
    my sub helper { my $n = 1; bump($n); $n }
    helper() ~ "/$n";
}
is nested-bump(), '2/100', 'a same-named variable of an older frame is untouched by the write';

my $g = 5;
sub reads-global { my $g = 9; my $r = isleap($g); $g }
is reads-global(), 9, 'a callee variable shadowing a mainline variable keeps its value';
is $g, 5, 'and the mainline variable is untouched';
