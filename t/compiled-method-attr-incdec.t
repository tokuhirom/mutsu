use Test;

# §B (#3658): a public rw attribute mutated by ++/--/pre-inc/dec inside a method
# body (`$.count++`) must persist to the instance even when the method runs as
# compiled bytecode (the writeback-safety gate now treats `.count` as an
# attribute, not a captured-outer free var, so the method dispatches compiled).
# Regression: it previously silently dropped the mutation (returned the old
# value) under compiled dispatch, breaking the hyper-rw `@objs».inc` form.

plan 13;

class Counter {
    has $.count is rw;
    method inc  { $.count++ }
    method dec  { $.count-- }
    method preinc { ++$.count }
    method predec { --$.count }
}

# Direct dispatch — post-increment returns OLD value, persists the new one.
my $c = Counter.new(count => 5);
is $c.inc, 5, 'post-increment returns the old value';
is $c.count, 6, 'post-increment persisted to the instance';

is $c.dec, 6, 'post-decrement returns the old value';
is $c.count, 5, 'post-decrement persisted to the instance';

# Pre-increment / pre-decrement return the NEW value.
is $c.preinc, 6, 'pre-increment returns the new value';
is $c.count, 6, 'pre-increment persisted to the instance';

is $c.predec, 5, 'pre-decrement returns the new value';
is $c.count, 5, 'pre-decrement persisted to the instance';

# Hyper dispatch (`@objs».inc`) updates each object's rw public attribute.
my @objs = (1..3).map({ Counter.new(count => $_) });
@objs».inc;
is @objs.map({ .count }).join(","), "2,3,4",
    'hyper post-increment updates rw public attributes';

@objs».dec;
is @objs.map({ .count }).join(","), "1,2,3",
    'hyper post-decrement updates rw public attributes';

# Multiple increments accumulate across calls (cell, not a per-call snapshot).
my $d = Counter.new(count => 0);
$d.inc for ^4;
is $d.count, 4, 'repeated increments accumulate on the shared cell';

# String increment semantics still apply to the attribute.
class Tag { has $.id is rw; method bump { $.id++ } }
my $t = Tag.new(id => 'aa');
$t.bump;
is $t.id, 'ab', 'string magic increment works on a public attribute';

# Private-attribute increment (the always-mutable storage form) still works.
class Priv { has $!n; method bump { $!n++ }; method get { $!n } }
my $p = Priv.new;
$p.bump; $p.bump;
is $p.get, 2, 'private attribute increment still persists';
