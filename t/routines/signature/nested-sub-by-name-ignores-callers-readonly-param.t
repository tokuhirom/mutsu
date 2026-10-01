use Test;

plan 3;

# #10400: a closure that merely CALLS a nested `my sub` by name (the sub writes
# the captured variable) must not see the caller's readonly same-named param.
sub mk-by-name() {
    my $s = 0;
    my sub bump() { $s++ }
    my $run = { bump(); $s };
    return $run;
}
sub cc-by-name($s) { mk-by-name()() }
is cc-by-name(1), 1, 'closure calling nested my sub by name';

sub mk-transitive() {
    my $s = 0;
    my sub a() { $s++ }
    my sub b() { a() }
    my $run = { b(); $s };
    return $run;
}
sub cc-transitive($s) { mk-transitive()() }
is cc-transitive(1), 1, 'transitive nested sub chain';

# A genuinely readonly parameter must still reject writes.
sub still-readonly($s) { my sub w() { $s++ }; { w() }() }
throws-like { still-readonly(1) }, X::Multi::NoMatch, 'own readonly param stays readonly';
