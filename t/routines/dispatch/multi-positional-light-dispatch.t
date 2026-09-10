use v6;
use Test;

# A `multi` candidate with a plain all-positional signature now runs through the
# same positional-light call entry an ordinary `sub` of that shape takes
# (`compile_and_call_function_def`, #7573). The entry is chosen per call, after
# the winner has already been resolved, and nothing about it is cached under the
# bare name -- these tests pin the behaviour that would break if it were.

plan 20;

multi sub tw(Int $x) { "int:$x" }
multi sub tw(Str $x) { "str:$x" }

# Repeated calls must re-resolve per argument type, not reuse the first winner.
is tw(1), 'int:1', 'Int candidate';
is tw('a'), 'str:a', 'Str candidate after Int';
is tw(2), 'int:2', 'Int candidate again';
is tw('b'), 'str:b', 'Str candidate again';

# Alternating in a loop is the shape a name-keyed light-call cache would break.
my @seen;
for 1 .. 4 -> $i {
    @seen.push(tw($i));
    @seen.push(tw("$i"));
}
is @seen.join(','),
    'int:1,str:1,int:2,str:2,int:3,str:3,int:4,str:4',
    'alternating types resolve independently on every call';

# A defaulted trailing positional (the shape `multi sub ok(Mu $c, $desc = '')`
# takes in the vendored Test module) still binds its constant fill.
multi sub deflt(Mu $cond, $desc = 'none') { "$cond/$desc" }
is deflt(1), '1/none', 'omitted default binds the constant fill';
is deflt(1, 'x'), '1/x', 'supplied argument wins over the default';

# `nextsame` from inside a candidate still reaches the next one: the
# multi-dispatch frame is pushed around the call, not inside the callee entry.
multi sub chain(Int $x) { nextsame }
multi sub chain(Any $x) { 'any' }
is chain(3), 'any', 'nextsame from a light-eligible candidate reaches the next one';

# `callsame` likewise, and hands the next candidate's value back to this one.
multi sub cs(Int $x) { 'int+' ~ callsame() }
multi sub cs(Any $x) { 'any' }
is cs(3), 'int+any', 'callsame returns the next candidate value';

# A candidate gets a fresh `$_`, not the caller's topic.
multi sub topicless(Int $x) { $_.defined ?? 'leaked' !! 'fresh' }
is (given 'outer' { topicless(1) }), 'fresh', 'candidate does not inherit the caller topic';

# Parameters are readonly.
multi sub ro(Int $x) { $x = 5; 'assigned' }
dies-ok { ro(1) }, 'a candidate parameter stays readonly';

# Return values are not containers leaking the callee slot.
multi sub ret(Int $x) { my $r = $x * 2; $r }
is ret(4), 8, 'candidate return value';

# Recursion through a multi keeps each frame's locals separate.
multi sub fact(Int $n where * <= 1) { 1 }
multi sub fact(Int $n) { $n * fact($n - 1) }
is fact(6), 720, 'recursive multi dispatch';

# A type constraint still rejects a non-matching argument.
multi sub only-int(Int $x) { $x }
my $wrong = 'nope';
dies-ok { only-int($wrong) }, 'no candidate for a wrong-typed argument';

# Definedness (`:D`/`:U`) still discriminates across repeated calls.
multi sub smiley(Int:D $x) { 'defined' }
multi sub smiley(Int:U $x) { 'undefined' }
is smiley(1), 'defined', 'Int:D candidate';
is smiley(Int), 'undefined', 'Int:U candidate';
is smiley(2), 'defined', 'Int:D candidate again';

# A candidate declared in a package resolves its own package's lexicals.
# `Cool` is a real type, not a wildcard: a user class instance does not do it.
# Reaching the light path made this observable, so pin both directions.
class Gadget { }
multi sub coolish(Cool $c) { 'cool' }
my $gadget = Gadget.new;
dies-ok { coolish($gadget) }, 'a Cool parameter rejects a user class instance';
is coolish(1), 'cool', 'a Cool parameter still accepts an Int';

module MPkg {
    our $tag = 'mpkg';
    our proto sub tagged(|) {*}
    multi sub tagged(Int $x) { "$tag:$x" }
}
is MPkg::tagged(7), 'mpkg:7', 'candidate body runs under its defining package';
