use Test;

plan 15;

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

# `.restore()` is idempotent -- a SECOND call on an already-restored handle
# is a no-op that answers `False`, not an error and not `True` again
# (todo/tickets/method-wrap-unwrap-restore-noop.md; the first `.restore()`
# above used to silently no-op by looking in the wrong map and reporting
# success regardless, and this second-call case threw "Invalid WrapHandle:
# not wrapped" instead of returning False). Verified against Rakudo v2026.06.
my $wh4 = Foo.^lookup('bar').candidates[0].wrap(-> $self, $x { callsame() + 100 });
is $wh4.restore, True, '.restore() on a live method-candidate handle returns True';
is $wh4.restore, False, 'a second .restore() on the same handle is a no-op returning False';
is $inst.bar(5), 10, 'the method stays unwrapped after a redundant .restore()';

# Same idempotency check for a plain sub wrap handle (the sibling code path).
sub plain-fn($x) { $x * 3 }
my $wh5 = &plain-fn.wrap(-> $x { callsame() + 1 });
is $wh5.restore, True, '.restore() on a live sub WrapHandle returns True';
is $wh5.restore, False, 'a second .restore() on the same sub handle is a no-op returning False';
