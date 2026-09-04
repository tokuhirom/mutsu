use Test;

# A `for` loop over a SCALAR source binds the variable's own container, not a
# copy of its value plus a writeback.
#
# The write-through itself already worked, through `store_loop_source_var` --
# which writes straight into the local slot and `env`, bypassing the container
# chokepoint entirely, so nothing type-checked it. ADR-0045's answer is to make
# the parameter a real alias and delete that writeback: the loop parameter binds
# the variable's `ContainerRef`, so a write through it is checked exactly like
# `$a = ...` is, and there is nothing left to write back.

plan 20;

subset SmallInt of Int where -128 <= $_ <= 127;

sub msg(&c) { my $m; { c(); CATCH { default { $m = .message.lines[0] } } }; $m // '' }

# --- 1. the write is type-checked, in every parameter shape -----------------
is msg({ my SmallInt $a = 1; for $a -> \x { x = 1000 } }),
  'Type check failed in assignment to $a; expected SmallInt but got Int (1000)',
  'a sigilless loop parameter type-checks its write';

is msg({ my SmallInt $a = 1; for $a -> $x is rw { $x = 1000 } }),
  'Type check failed in assignment to $a; expected SmallInt but got Int (1000)',
  'and so does `is rw`';

is msg({ my SmallInt $a = 1; for $a <-> $x { $x = 1000 } }),
  'Type check failed in assignment to $a; expected SmallInt but got Int (1000)',
  'and `<->`';

is msg({ my Int $a = 1; for $a -> \x { x = "s" } }),
  'Type check failed in assignment to $a; expected Int but got Str ("s")',
  'a plain type constraint is checked too';

is msg({ my SmallInt $a = 1; my \x := $a; x = 1000 }),
  'Type check failed in assignment to $a; expected SmallInt but got Int (1000)',
  'control: the `:=` spelling already did';

# --- 2. ... and a well-typed write still goes through -----------------------
{
    my SmallInt $a = 1;
    for $a -> \x { x = 9 }
    is $a, 9, 'a well-typed write through the alias lands';
}
{
    my $a = 1;
    for $a -> \x { x = 9 }
    is $a, 9, 'an untyped scalar takes anything';
}
{
    my $a = 1;
    for $a -> $x is rw { $x = 9 }
    is $a, 9, '`is rw` writes through';
}
{
    my $a = 1;
    for $a <-> $x { $x = 9 }
    is $a, 9, '`<->` writes through';
}
{
    my $a = 1;
    for $a { $_ = 9 }
    is $a, 9, 'the implicit topic writes through';
}
{
    my $a = 1;
    for $a -> \x { x = 9; x = 10 }
    is $a, 10, 'two writes in one iteration both land';
}
{
    my $a;
    for $a -> \x { x = 5 }
    is $a, 5, 'an undefined scalar is written';
}

# --- 3. a read-only binding is still read-only ------------------------------
{
    my $a = 1;
    for $a -> $x { }
    is $a, 1, 'a plain `-> $x` leaves the source alone';
}
like msg({ my $a = 1; for $a -> $x { $x = 9 } }),
  /'readonly'/, 'and refuses the write';
{
    my $a = 1;
    for $a -> \x { is x, 1, 'the alias reads the value' }
}

# --- 4. shapes that must NOT alias the variable -----------------------------
{
    # A derived producer on the variable yields the pair's VALUE, not `$pair`.
    my $pair = (a => my $ = 42);
    for $pair.value -> $v is rw { $v += 1 }
    is $pair.value, 43, 'a `.value` producer aliases the value, not the variable';
    isa-ok $pair, Pair, 'and the variable is still a Pair';
}
{
    # A mutable QuantHash `.values` binds WEIGHTS (ADR-0045 §2.4).
    my $b = <a a a>.BagHash;
    for $b.values { $_-- }
    is $b<a>, 2, 'a BagHash `.values` still writes the weight';
}
{
    # `for @$s` iterates the scalar's INNER array.
    my $s = [1, 2];
    for @$s -> $x is rw { $x = 9 }
    is-deeply $s, $[9, 9], 'the deref`d-container shape still aliases elements';
}
{
    # A scalar holding an array is ONE item; the alias is the variable.
    my $a = [1, 2];
    for $a -> \x { x = 9 }
    is $a, 9, 'a scalar holding an array binds the scalar itself';
}
