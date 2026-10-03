use Test;

# A callee's sigilless parameter that shares its name with the caller's must
# not retarget the caller's alias: the caller's return-side writeback would
# otherwise overwrite the callee's argument variable with its own value.

plan 4;

sub leaf(\c) { c<a> = "NEW"; }
sub top(\c) { my $root = c.clone; leaf($root); $root }
my %h = :a<old>;
is-deeply top(%h), {a => "NEW"}, 'write through nested same-named \c reaches the clone';
is-deeply %h, {a => "old"}, 'original container is unchanged';

sub top2(\c) { my $root = c.clone; leaf($root); c<b> = 1; $root }
my %h2 = :a<old>;
is-deeply top2(%h2), {a => "NEW"}, 'caller keeps its own alias after the inner call';
is-deeply %h2, {a => "old", b => 1}, 'caller write reaches its own argument';
