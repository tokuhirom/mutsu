use Test;

plan 6;

# A `my` declared in a loop BODY is block-scoped: it must not survive the loop
# as a name in the enclosing scope. mutsu's loop-scope exit used to restore only
# names that *shadowed* an outer binding, so a body-local `my` with no enclosing
# namesake stayed behind under its bare name — visible to symbolic lookup, and
# (once any method-env merge propagated it) able to overwrite an unrelated
# frame's same-named variable.
#
# The assertion is "the body's value is not reachable", not "the lookup is Nil":
# rakudo answers a lowered-away lexical with a
# `Rakudo::Internals::LoweredAwayLexical` sentinel rather than Nil, and both
# answers mean the same thing here.

for 1..1 { my $blk-a = 4; }
isnt (try ::('$blk-a')), 4,
    'a `for` body `my` is not reachable after the loop';

loop (my $n = 0; $n < 1; $n++) { my $blk-b = 5; }
isnt (try ::('$blk-b')), 5,
    'a C-style `loop` body `my` is not reachable after the loop';

my $m = 0;
while $m++ < 1 { my $blk-c = 6; }
isnt (try ::('$blk-c')), 6,
    'a `while` body `my` is not reachable after the loop';

# A native-typed declaration mutated by the body takes the same route.
for 1..1 { my int $blk-d = 3; while --$blk-d >= 0 { } }
isnt (try ::('$blk-d')), -1,
    'a mutated native-typed body `my` is not reachable after the loop';

# The shadow half must keep working: an enclosing binding of the same name is
# restored, not removed.
my $outer = 'kept';
for 1..1 { my $outer = 'inner'; }
is $outer, 'kept', 'a body `my` shadowing an outer binding restores it';

# A `state` in a loop body is re-executed every iteration but denotes ONE
# binding that accumulates across them — it must not be swept away with the
# body's `my`s. (zef's `@*ARGS` reordering is exactly this shape, down to the
# `LAST` phaser reading the accumulated state.)
my @args = <--version foo --bar>;
for @args -> $arg {
    state @positional;
    state @named;
    LAST { @args = flat @named, @positional; }
    $arg.starts-with('-') ?? @named.append($arg) !! @positional.append($arg);
}
is @args.join(' '), '--version --bar foo',
    'a `state` in a loop body accumulates across iterations and survives it';
