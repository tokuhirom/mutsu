use v6;
use Test;

plan 7;

# Rakudo's `whenever`-scope check is purely lexical: a `whenever` is valid as
# long as some `supply`/`react` block encloses it in the source, at ANY nesting
# depth — routine/closure boundaries do NOT break the enclosure. mutsu reset the
# scope at every sub/method/pointy boundary, wrongly rejecting valid code like
# `supply { my sub g { whenever … } }` (seen in IO::Notification::Recursive).
# It also lacked the `supply whenever …` statement shorthand (seen in AI::Gator).

# --- Parse-acceptance: these must compile without X::Comp::WheneverOutOfScope. ---
lives-ok { EVAL 'supply { my sub relay($s) { whenever $s -> $v { emit $v } }; relay(Supply.interval(1)); }' },
    'whenever in a sub nested in a supply compiles';
lives-ok { EVAL 'supply { my &f = -> $s { whenever $s -> $v { emit $v } }; f(Supply.interval(1)); }' },
    'whenever in a pointy nested in a supply compiles';
lives-ok { EVAL 'supply whenever Supply.interval(1) -> $v { emit $v }' },
    'supply whenever shorthand compiles';

# --- These must still be rejected (no supply/react ancestor). ---
throws-like 'sub g($x) { whenever $x -> $e { } }; g(Supply.interval(1))',
    X::Comp::WheneverOutOfScope,
    'whenever in a top-level sub is rejected';
throws-like 'my &f = -> $x { whenever $x -> $e { } }',
    X::Comp::WheneverOutOfScope,
    'whenever in a top-level pointy is rejected';

# --- Runtime: the working forms emit correctly. ---
my @a;
my $s = supply whenever Supply.from-list(10, 20) -> $v { emit $v + 1 }
$s.tap({ @a.push($_) });
is-deeply @a, [11, 21], 'supply whenever shorthand emits';

my @b;
react whenever Supply.from-list(5, 6) -> $v { @b.push($v); }
is-deeply @b, [5, 6], 'react whenever shorthand still works';
