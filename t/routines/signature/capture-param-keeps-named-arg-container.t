use Test;

# A `|c` capture keeps the caller's container of a NAMED argument too, so
# forwarding it (`root |c`) reaches an `is raw` named parameter writably
# (#16121, Deps' `deps-root`). Checked against rakudo.

plan 8;

sub root(:$deps is raw) { $deps = 5 }
sub w(|c) { root |c }

my $z;
w :deps($z);
is $z, 5, 'a named argument forwarded through |c writes the caller variable';

w(:deps(my $y));
is $y, 5, '...also for a variable declared in the argument';

my $u;
my &wl = -> |c { root |c };
wl :deps($u);
is $u, 5, '...through a pointy block taking |c';

multi root2(&b, :$deps is raw, Any:U :$class, |c) { $deps = 6; b() }
my &wm = -> |c { root2 |c, :class(Any) };
my $seen;
wm :deps(my $m), { $seen = $m };
is $seen, 6, 'a closure written beside the declaration sees the write';

sub pos($d is raw) { $d = 6 }
sub wp(|c) { pos |c }
my $p;
wp $p;
is $p, 6, 'the positional form still works';

sub base($obj, &block, :$deps is raw) {
    $deps = $obj if not $deps.defined;
    block |($obj if &block.arity);
}
my $serial = 100;
sub deps-root(&block, :$deps is raw, |c) { base $serial++, &block, :$deps, |c }
my &dr = -> |c { deps-root |c };
my @seen;
dr :deps(my $r1), {
    dr :deps(my $r2), {
        @seen = $r1.defined, $r2.defined, $r1 != $r2;
    }
}
is-deeply @seen, [True, True, True], 'nested roots keep distinct, defined variables';

my $k;
my &w2 = -> |c { root |c };
w2 :deps($k);
is $k, 5, 'a pre-declared variable through a pointy block';
w2 :deps($k);
is $k, 5, 'and a second call';
