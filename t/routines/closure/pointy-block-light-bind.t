use v6;
use Test;

# A single-parameter pointy block (`-> $a { ... }`) carries no ParamDef: its
# signature survives only as the parameter NAME, which is why it reached the
# general signature binder's legacy placeholder branch. #8335 gave that shape a
# light bind that writes the argument straight into the parameter's env key.
# These tests pin the observable behaviour the light bind has to reproduce
# exactly, and the shapes it must decline so the general binder still runs.

plan 26;

# --- the basic bind ---------------------------------------------------------

my $id = -> $a { $a };
is $id(41) + 1, 42, 'a pointy block binds its single positional argument';
is $id('x'), 'x', '...whatever the argument type';

# A plain `$` parameter is an ITEM binding: a Positional argument is itemized,
# so `.raku` renders the `$` container the parameter is.
my @arr = 1, 2, 3;
is $id(@arr).raku, '$[1, 2, 3]', 'an array argument is itemized on bind';
is $id(@arr).WHAT.gist, '(Array)', '...and stays an Array';

my %h = a => 1;
is $id(%h).raku, '${:a(1)}', 'a hash argument is itemized on bind';

# --- a Callable argument ----------------------------------------------------

my $apply = -> $cb { $cb(7) };
is $apply(-> $n { $n * 3 }), 21, 'a Callable parameter is invocable';
is $apply(&sqrt), 7.sqrt, '...including a named routine passed by reference';

# --- readonly ---------------------------------------------------------------

my $writer = -> $a { $a = 1 };
dies-ok { $writer(1) }, 'a pointy-block parameter stays readonly';

# --- arity ------------------------------------------------------------------

# An arity mismatch is NOT the light bind's to diagnose: it declines and the
# general binder reports it. (mutsu's legacy binder reports a SHORT call as an
# undeclared-variable read rather than rakudo's "Too few positionals" -- issue
# #8353, a divergence that predates the light bind -- so only the dying is
# asserted here.)
my $two = -> $a { $a };
dies-ok { $two() }, 'a short call still dies';
throws-like { $two(1, 2) }, Exception, message => /'Too many positionals'/,
    'a surplus call still reports "Too many positionals"';

# --- argument kinds the light bind declines ---------------------------------

# A `Pair` is a NAMED argument: the general binder keeps it out of the
# positional list, so this call supplies no positional at all.
dies-ok { $two(:k(1)) }, 'a named argument does not fill a positional parameter';

# A colonpair written positionally is a `ValuePair`, which DOES bind
# positionally for a plain-positional signature.
is $id((k => 1)).raku, ':k(1)', 'a positional colonpair binds to the parameter';

# --- captures ---------------------------------------------------------------

my $n = 5;
my $adder = -> $a { $a + $n };
is $adder(1), 6, 'a free variable is still visible to a light-bound body';
$n = 10;
is $adder(1), 11, '...and tracks the caller-side mutation';

my $sink = 0;
my $store = -> $a { $sink = $a };
$store(9);
is $sink, 9, 'a light-bound body still writes a captured lexical back';

# A nested closure must still capture the parameter.
my $outer = -> $a { -> { $a } };
is $outer(3)(), 3, 'a nested closure captures the light-bound parameter';

# The parameter must not leak into the caller under its own name.
my $a = 'caller';
$id(1);
is $a, 'caller', 'the parameter does not leak back into a same-named caller lexical';

# --- the topic --------------------------------------------------------------

$_ = 'topic';
is $id(1), 1, 'binding a parameter does not disturb the caller topic';
is $_, 'topic', '...and the topic survives the call';

# --- the native list loops --------------------------------------------------

is (1, 2, 3).map(-> $x { $x * 2 }).join(','), '2,4,6', '.map over a pointy block';
is (1, 2, 3).grep(-> $x { $x > 1 }).join(','), '2,3', '.grep over a pointy block';
is (3, 1, 2).sort(-> $x { $x }).join(','), '1,2,3', '.sort over a pointy block';
is (1, 2, 3).first(-> $x { $x > 1 }), 2, '.first over a pointy block';

# A `.map` over Pair-shaped elements hands the block its element as a pair; the
# block must still see it.
my %pairs = a => 1;
is %pairs.map(-> $p { $p.key }).join(','), 'a', '.map over hash pairs binds the element';

# --- recursion --------------------------------------------------------------

my $fact;
$fact = -> $i { $i <= 1 ?? 1 !! $i * $fact($i - 1) };
is $fact(5), 120, 'a self-recursive pointy block binds each frame separately';

# --- WhateverCode is NOT light-bound (its parameter is the topic) ------------

is (1, 2, 3).map(* + 1).join(','), '2,3,4', 'WhateverCode still binds through the topic';
