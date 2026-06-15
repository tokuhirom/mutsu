use Test;

# Pins the CP-3 collapse PoC: `vm_run_block_raw` (deferred role-body statements)
# now runs in-place on the VM via `run_nested` instead of the
# `mem::take`/`VM::new` ping-pong. The nested run must (a) execute correctly,
# (b) see/mutate shared state, and (c) leave the surrounding VM execution
# registers (outer locals, arrays, loop state) intact.

plan 8;

# Outer state established before a role whose body runs deferred statements.
my @outer = 10, 20, 30;
my $sum = 0;
$sum += $_ for @outer;
is $sum, 60, 'outer loop computed before role decl';

my $side = 0;
role R {
    method label { "R-label" }
    # deferred (non-declaration) body statements run during registration via
    # vm_run_block_raw -> run_nested:
    $side = @outer.elems * 100;     # reads outer lexical + mutates outer scalar
    my $inner = $side + 1;          # nested-scope local
}

# Nested run saw the shared outer state and mutated it:
is $side, 300, 'deferred role body read @outer and mutated outer $side';

# Outer execution registers intact after the nested run:
is @outer.elems, 3, 'outer array survived nested run';
is @outer[1], 20, 'outer array contents intact';
is $sum, 60, 'outer scalar intact';

# A loop after the role still works (outer loop machinery not corrupted):
my @doubled = @outer.map(* * 2);
is @doubled.join(","), "20,40,60", 'map after nested run works';

# The role is usable and composable:
class C does R { }
is C.new.label, "R-label", 'class composing the role works';
is R ~~ Mu, True, 'role is a type object';
