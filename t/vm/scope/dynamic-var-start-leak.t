use Test;

plan 5;

# A dynamic bound (:=) while a `start` block is spawned must not leak into
# the parent's lineage after the frame returns — dynamics are thread-local
# in Raku, and the shared-store seeding used for `start` (to give a spawned
# worker visibility into ordinary lexicals) must exclude dynamics.
sub s1() { my $*A := 1; start { 0 } }
await s1();
is (try $*A).raku, 'Nil', 'bound dynamic does not leak after await start';

sub s2() { my $*B := 2; start { 0 }; Nil }
s2();
is (try $*B).raku, 'Nil', 'bound dynamic does not leak without an await';

sub s4() { my $*D = 4; start { 0 } }
await s4();
is (try $*D).raku, 'Nil', 'assigned (not bound) dynamic does not leak either';

# The start body itself still reads the dynamic fine (its env is a clone of
# the parent's at spawn time) — only cross-thread name-lane sharing stops.
sub s3() {
    my $*C := 3;
    await start { $*C };
}
is s3(), 3, 'a start block still reads the dynamic that was live at spawn time';

# No leak when start runs BEFORE the dynamic is bound.
sub s5() { start { 0 }; my $*E := 5; }
s5();
is (try $*E).raku, 'Nil', 'no leak when start runs before the binding';

done-testing;
