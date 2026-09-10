use Test;

# Pin for the closure-creation fast path: `capture_closure_env` now builds the
# free-variable and own-locals membership sets ONCE per compiled chunk
# (`CompiledCode::capture_free_var_set` / `capture_local_set`) instead of
# re-collecting them on every closure creation, and tests the own-locals set by
# interned `Symbol` rather than by string. Both sets decide what a closure
# inherits from the frame that creates it, so the semantics they encode are what
# this file pins — a stale or mis-keyed set would show up here.

plan 17;

# --- Free variables are captured, and captured per creation ------------------
my @made;
for 1, 2, 3 -> $i {
    @made.push: { $i * 10 };
}
is @made.map({ $_.() }).join(','), '10,20,30',
    'each closure created in a loop keeps its own captured free variable';

my $outer = 5;
my $adder = -> $n { $n + $outer };
is $adder(1), 6, 'a free variable is captured';
$outer = 50;
is $adder(1), 51, 'the capture tracks a later mutation of the outer binding';

# --- The closure's OWN locals must not inherit the creating frame's binding ---
# A WhateverCode's `_` param is in `cc.locals`; the enclosing topic must not
# leak into it, and must not leak back out to the caller afterwards.
$_ = 'outer-topic';
my $wc = * ~~ /o/;
is $_, 'outer-topic', 'creating a WhateverCode leaves the caller topic alone';
ok $wc('foo'), 'the WhateverCode binds its own argument, not the outer topic';
is $_, 'outer-topic', 'calling it still leaves the caller topic alone';

for <a b> {
    my $inner = { $_ };
    is $inner('x'), 'x', "a block's own \$_ param wins over the enclosing topic ($_)";
}

# A same-named local declared inside the block shadows the outer one.
my $shadowed = 'outer';
my $blk = { my $shadowed = 'inner'; $shadowed };
is $blk(), 'inner', 'a block-local declaration shadows the same-named outer lexical';
is $shadowed, 'outer', 'and does not write through to the outer one';

# --- Names that are NOT plain user lexicals stay visible ---------------------
# Uppercase-initial lexicals, dynamics, `self` and type names all have to remain
# reachable from inside a closure.
my $Uppercase = 'U';
is { $Uppercase }(), 'U', 'an uppercase-initial lexical is visible in a closure';

my $*DYN = 'D';
is { $*DYN }(), 'D', 'a dynamic variable is visible in a closure';

is { Int }().gist, '(Int)', 'a type name resolves inside a closure';

class Holder {
    has $.v;
    method wrapped() { return { self.v } }
}
is Holder.new(v => 7).wrapped().(), 7, 'self is reachable through a captured closure';

# --- The same chunk creating many closures (the cached-set path) --------------
sub make($n) { return { $n * 2 } }
my @fns = (1 .. 4).map({ make($_) });
is @fns.map({ $_.() }).join(','), '2,4,6,8',
    'many closures from one chunk each capture their own value';

# A nested closure created inside another closure.
sub outer-factory($a) { return -> $b { -> { $a + $b } } }
is outer-factory(10)(5)(), 15, 'a closure created inside a closure captures both levels';

# A WhateverCode created inside a block must not be mistaken for the block's own
# callable kind (the `__mutsu_callable_type` marker must not be inherited).
my $blockish = { $_ };
is $blockish.WHAT.gist, '(Block)', 'a bare block is a Block, not a WhateverCode';
