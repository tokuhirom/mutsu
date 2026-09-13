use v6;
use Test;

# The JSON modules go through the ordinary module-resolution ladder (#8183).
#
# mutsu used to recognize the bare names `JSON::Fast` and `JSON::Tiny` at
# `use`-time and answer both from one native Rust implementation
# (src/runtime/json.rs), *before* the `use lib` / `-I` / `MUTSULIB` / installed
# / bundled search ran and even ahead of an already-resolved routine def. That
# made two things observable that should not have been: which implementation a
# program got was decided by the module's name rather than by the ladder
# (BATTERIES.md §6), and `from-json`'s exception shape was guessed from the
# *set* of module names the program had `use`d.
#
# Now both are vendored batteries that always resolve to real Raku code
# (`JSON::Tiny` since #8183, `JSON::Fast` since #8226), and there is no native
# JSON provider left for either name to reach. The only JSON mutsu still
# answers from Rust is `Rakudo::Internals::JSON`, a core class that no `use`
# gates -- see the last case below.
#
# Each case runs in its own process: `use` is global, and the point of the test
# is what a *fresh* program sees.

plan 8;

my $mutsu = $*EXECUTABLE.absolute;
my $fixture = 't/fixtures/json-ladder/lib';

sub run-snippet($code, *%opts) {
    my $proc = run $mutsu, '-e', $code, :out, :err, |%opts;
    my $out = $proc.out.slurp(:close).trim;
    my $err = $proc.err.slurp(:close).trim;
    return ($out, $err);
}

# --- JSON::Tiny resolves to the bundled battery, not to a native stand-in ---

{
    my ($out, $err) = run-snippet('use JSON::Tiny; print to-json([1, 2, "x"])');
    is $out, '[ 1, 2, "x" ]',
        'use JSON::Tiny runs the vendored module (spaced list, not a pretty block)';
    is $err, '', 'no warning on the bundled JSON::Tiny path';
}

# --- ... and an explicit -I copy outranks the bundled battery ---

{
    # sanity: without -I, the bundled battery answers
    my ($out, $) = run-snippet('use JSON::Tiny; print to-json([1, 2])');
    is $out, '[ 1, 2 ]', 'bundled JSON::Tiny is the floor';
}

{
    my $proc = run $mutsu, '-I', $fixture, '-e',
        'use JSON::Tiny; print to-json([1, 2])', :out, :err;
    my $out = $proc.out.slurp(:close).trim;
    $proc.err.slurp(:close);
    is $out, 'ladder-to-json:1,2',
        '-I JSON::Tiny shadows the bundled battery (BATTERIES.md 6)';
}

# --- JSON::Fast resolves to the bundled battery, and -I outranks it ---

{
    my ($out, $) = run-snippet('use JSON::Fast; print from-json(q<{"a":1}>).raku');
    is $out, '{:a(1)}',
        'use JSON::Fast runs the vendored module';
}

{
    my $proc = run $mutsu, '-I', $fixture, '-e',
        'use JSON::Fast; print from-json(q<{"a":1}>)', :out, :err;
    my $out = $proc.out.slurp(:close).trim;
    $proc.err.slurp(:close);
    is $out, 'ladder-from-json:{"a":1}',
        '-I JSON::Fast shadows the bundled battery (BATTERIES.md 6)';
}

# --- the exception shape no longer depends on which names were `use`d ---

# Loading JSON::Fast alongside JSON::Tiny used to flip JSON::Tiny's parse
# failure to a JSON::Fast-shaped error. Each module owns its own error now.
{
    my ($out, $) = run-snippet(
        'use JSON::Fast; use JSON::Tiny; try { from-json("") }; print $!.^name');
    is $out, 'JSON::Tiny::X::JSON::Tiny::Invalid',
        'JSON::Tiny keeps its own exception even with JSON::Fast also loaded';
}

# --- the one JSON mutsu still answers from Rust is the CORE class ---

# `Rakudo::Internals::JSON` resolves with no `use` in rakudo too, so answering
# it natively is core surface rather than a BATTERIES.md rung-3 provider. zef
# reads every META6.json through it (vendor/zef/lib/Zef.rakumod).
{
    my ($out, $) = run-snippet(
        'print Rakudo::Internals::JSON.from-json(q<{"a":1}>).raku');
    is $out, '{:a(1)}',
        'Rakudo::Internals::JSON needs no `use` and survived the provider removal';
}
