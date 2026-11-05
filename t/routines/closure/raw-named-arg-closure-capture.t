use Test;
plan 6;

# A named argument bound to an `is raw` / `is rw` parameter writes the
# caller's variable; a closure created earlier must observe the write
# (#16121).
sub w(:$p is raw) { $p = 5 }

my $a; my &c1 = { $a };
w :p($a);
is c1(), 5, 'closure sees write through a named is-raw argument';

my $b; my &c2 = { $b };
w(p => $b);
is c2(), 5, 'fat-arrow named argument form';

sub run(&blk, :$deps is raw) { $deps = 7; blk() }
is (run :deps(my $r), { $r }), 7, 'block sees variable declared in the named argument';
is $r, 7, 'declared variable holds the value after the call';

multi set-deps($d is rw, $v) { $d = $v if not $d.defined }
multi set-deps($, $) {}
sub base($o, &blk, :$deps is raw) { set-deps $deps, $o; blk() }
sub root(&blk, :$deps is raw, |c) { base 3, &blk, :$deps, |c }
is (root :deps(my $r1), { $r1 }), 3, 'forwarded through a slurpy-capturing routine';

my $x; my &c3 = { $x };
sub pos($p is raw) { $p = 9 }
pos $x;
is c3(), 9, 'positional form still works';
