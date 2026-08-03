use v6;
use Test;

# Once a program spawns a thread, `shared_vars_active` stays on for the rest of
# the process and every plain lexical `@a.push` funnels through the name-keyed
# `__mutsu_atomic_arr::` store. Seeding that store read the env binding's view
# without dereferencing it -- and a lexical a closure captures (which is what a
# module's file-scope `my @a` is, as seen from its own subs) is a `ContainerRef`
# cell, not a bare Array. Neither arm matched, so the atomic entry was seeded
# EMPTY and everything already in the array was dropped; the write-back then
# replaced the cell with a bare Array, detaching every other holder.
#
# Under the real Test.rakumod that is `@vars`, the subtest stack: a file that
# spawned a thread mid-subtest lost the outer frame and died with "Cannot pop
# from an empty Array" (roast/S02-types/capture.t).

plan 6;

my $dir = $*TMPDIR.child("mutsu-shared-arr-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

$dir.child('SharedArr.rakumod').spurt(
    'unit module SharedArr;
my @vars;
sub sh-push($v) { @vars.push: $v }
sub sh-elems() is export { @vars.elems }
sub nest($v, &body) is export {
    sh-push($v);
    my $r = body();
    (@vars.pop, $r)
}
');

my $lib = 'use lib "' ~ $dir.absolute ~ '";' ~ "\n";

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim.subst("\n", " ", :g)
}

# The array already holds one element when the thread spawns, and the push that
# follows must not lose it.
is run-snippet('mid.raku', $lib ~ 'use SharedArr;
nest 1, {
    await start { 1 };
    nest 2, { say sh-elems() };
};
'), '2', 'a push after a mid-flight spawn keeps what the array already held';

# The pop after that spawn must return the element the matching push added, and
# leave the outer one in place.
is run-snippet('pop.raku', $lib ~ 'use SharedArr;
my $outer = nest 1, {
    await start { 1 };
    nest 2, { 0 };
};
say $outer[0];
say sh-elems();
'), '1 0', 'the outer frame is still poppable and the array drains to empty';

# The cell identity has to survive: a second module sub reading the same
# file-scope `@vars` must see the push, not a frozen pre-spawn copy.
is run-snippet('identity.raku', $lib ~ 'use SharedArr;
await start { 1 };
nest 1, { say sh-elems() };
say sh-elems();
'), '1 0', 'another sub reading the same lexical sees the mutation';

# Deeper nesting across the spawn.
is run-snippet('deep.raku', $lib ~ 'use SharedArr;
nest 1, {
    nest 2, {
        await start { 1 };
        nest 3, { say sh-elems() };
    };
    say sh-elems();
};
'), '3 1', 'nested frames across a spawn all survive';

# The in-process shape of the same thing, without a module: a `@a` captured by a
# closure is celled too.
my @a;
my &grow = { @a.push: $^v };
grow(1);
await start { 1 };
grow(2);
is @a.elems, 2, 'a closure-captured array keeps its contents across a spawn';
is-deeply @a[0], 1, 'and the pre-spawn element is still the first one';
