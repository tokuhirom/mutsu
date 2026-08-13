use Test;

plan 10;

# ADR-0019 E10: `.wrap()` on a method CANDIDATE (`^lookup(...).candidates[N]`,
# not a plain sub `&foo`) stores its wrapper in `Registry::method_wrap_chains`,
# keyed by (class, method, candidate index) rather than by sub id. Before this
# fix, neither `.restore()` on the returned WrapHandle nor `.unwrap($handle)`
# on the candidate itself ever removed the entry -- `.restore()` silently
# looked in the wrong (sub-id-keyed) map and still reported success, and
# `.unwrap()` on the candidate fell through to sub-level logic and always
# raised "not wrapped". Verified against Rakudo v2026.06 (`raku`).
class Foo {
    method bar($x) { return $x * 2; }
    multi method baz(Int $x) { return $x + 1; }
}

my $inst = Foo.new;

is $inst.bar(5), 10, 'unwrapped candidate: control value';

my $wh = Foo.^lookup('bar').candidates[0].wrap(-> $self, $x { callsame() + 100 });
is $inst.bar(5), 110, 'wrapped candidate applies the wrapper';
$wh.restore;
is $inst.bar(5), 10, '.restore() on a candidate WrapHandle actually removes the wrapper';

my $cand = Foo.^lookup('bar').candidates[0];
my $wh2 = $cand.wrap(-> $self, $x { callsame() + 100 });
is $inst.bar(5), 110, 're-wrapped candidate applies the wrapper again';
lives-ok { $cand.unwrap($wh2) }, '.unwrap(handle) on a candidate lives';
is $inst.bar(5), 10, '.unwrap(handle) on a candidate actually removes the wrapper';

dies-ok { $cand.unwrap($wh2) }, 'unwrapping an already-removed handle dies';

# A multi candidate obtained via `^lookup` behaves the same way.
is $inst.baz(1), 2, 'unwrapped multi candidate: control value';
my $wh3 = Foo.^lookup('baz').candidates[0].wrap(-> $self, $x { callsame() + 1000 });
is $inst.baz(1), 1002, 'wrapped multi candidate applies the wrapper';
$wh3.restore;
is $inst.baz(1), 2, '.restore() on a multi candidate WrapHandle removes the wrapper';
