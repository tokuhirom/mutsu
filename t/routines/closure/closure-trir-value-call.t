use Test;

# A `-> $x { ... }` whose body TRIR proves runs as TRIR from a value call
# (#8335). These pin that the TRIR entry answers what the untyped closure
# entry answers, for each thing the entry has to get right.

plan 27;

# Plain results through a value call, a `.map` and a `.sort`.
{
    my $id  = -> $a { $a };
    my $inc = -> $n { $n + 1 };
    my $s = 0;
    for ^100 { $s = $inc($s) }
    is $s, 100, 'a value call in a loop accumulates';
    is $id(42), 42, 'identity block';
    is $id('str'), 'str', 'a Str argument';
    is (1, 2, 3).map(-> $x { $x * 2 }).join(','), '2,4,6', 'a pointy block as a map callback';
    is $inc(1.5), 2.5, 'a Rat argument';
}

# The parameter is an item binding of the argument.
{
    my @a = 1, 2, 3;
    my $id = -> $a { $a };
    my $n = 0;
    for $id(@a) { $n++ }
    is $n, 1, 'an Array argument comes back itemized';
    is (-> $a { $a.elems })(@a), 3, 'the bound Array is the argument';
    my $c = 0;
    $c++ for $id((4, 5));
    is $c, 1, 'a List argument comes back itemized';
}

# The parameter is read-only.
{
    my $set = -> $v { $v = 1 };
    throws-like { $set(5) }, Exception, message => /readonly/, 'assigning the parameter dies';
}

# Free variables are the capture's.
{
    my $offset = 10;
    my $add = -> $x { $x + $offset };
    is $add(1), 11, 'a free variable reads its capture';
    $offset = 20;
    is $add(1), 21, 'a later write to the free variable is seen';
    sub call-with-shadow(&f) { my $offset = 1000; f(1) }
    is call-with-shadow($add), 21, 'a same-named caller variable does not shadow the capture';
}
{
    my @blocks;
    for 1..3 -> $i { @blocks.push(-> $x { $x * $i }) }
    is @blocks.map({ $_(10) }).join(','), '10,20,30', 'each loop iteration keeps its own capture';
}

# Call shapes the TRIR entry does not take still work.
{
    my $id = -> $a { $a };
    is $id(any(1, 2)).^name, 'Junction', 'a Junction argument binds whole (no autothreading)';
    throws-like { $id() }, Exception, 'too few arguments dies';
    throws-like { $id(1, 2) }, Exception, 'too many arguments dies';
    my $add = -> $x { $x + 1 };
    my $two = $add.assuming(1);
    is $two(), 2, '.assuming';
}

# A body TRIR declines keeps the untyped entry.
{
    sub outer-ret() { my $b = -> $x { return $x * 3 }; $b(2); 'not reached' }
    is outer-ret(), 6, 'a return in a pointy block leaves the enclosing routine';
    is (-> $x { $x ~ '!' })('hi'), 'hi!', 'string concatenation';
}

# Errors from the body report from inside the block.
{
    my $num = -> $s { $s + 1 };
    throws-like { $num('abc') }, X::Str::Numeric, 'a failing coercion in the body dies';
    my $div = -> $d { 1 div $d };
    throws-like { $div(0) }, X::Numeric::DivideByZero, 'div by zero dies';
}

# `&?ROUTINE` inside a block called through the TRIR door is the enclosing routine.
{
    sub named-outer() { my $b = -> $x { &?ROUTINE.name }; $b(1) }
    is named-outer(), 'named-outer', '&?ROUTINE looks past the block';
}

# A pointy block with several parameters.
{
    my $cmp = -> $a, $b { $b <=> $a };
    is (3, 1, 2).sort($cmp).join(','), '3,2,1', 'a two-parameter block as a sort comparator';
    is (1..5).reduce(-> $a, $b { $a + $b }), 15, 'a two-parameter block as a reduce callback';
    my $typed = -> Int $x, $y { $x + $y };
    is $typed(1, 2), 3, 'a typed parameter';
    throws-like { $typed('a', 2) }, X::TypeCheck::Binding, 'a failed type check is the binder\'s error';
    is (-> { 42 })(), 42, 'a block with no parameters';
}
