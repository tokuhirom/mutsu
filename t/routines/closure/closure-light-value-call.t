use Test;

# A pointy block whose body is made of pure value ops runs through the light
# closure call (#8335, `vm_closure_light_call.rs`), and so do a WhateverCode
# and a placeholder block with such a body (#13286). These pin that it answers
# what the full closure entry answers, for each thing the entry has to get
# right, and that the shapes it declines still work.

plan 58;

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

# A body the light call declines keeps the full entry.
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

# `&?ROUTINE` inside a block is the enclosing routine.
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

# Arguments the light call declines, and bodies that reach outside.
{
    my $id = -> $a { $a };
    is $id(Nil).raku, 'Nil', 'a Nil argument';
    my $x = 5;
    my $cell = -> $v { $v };
    is $cell($x), 5, 'a variable argument';
    my class Pt { has $.v; method Str { "pt{$!v}" } }
    multi infix:<+>(Pt $a, Pt $b) { Pt.new(v => $a.v + $b.v) }
    is (-> $p, $q { $p + $q })(Pt.new(v => 1), Pt.new(v => 2)).v, 3, 'a user infix reached from the body';
    is (-> $p { $p ~ '!' })(Pt.new(v => 7)), 'pt7!', 'a user .Str reached from the body';
    my @log;
    my $push = -> $v { @log.push($v) };
    $push(1); $push(2);
    is @log.join(','), '1,2', 'a body that calls a method still runs';
    my $count = 0;
    my $bump = -> $n { $count += $n };
    $bump(3); $bump(4);
    is $count, 7, 'a body that writes a free variable still writes it back';
}

# A WhateverCode (#13286).
{
    my &f = * + 1;
    my $s = 0;
    for ^100 { $s = f($s) }
    is $s, 100, 'a WhateverCode called by name in a loop';
    is (1, 2, 3).map(* * 2).join(','), '2,4,6', 'a WhateverCode as a map callback';
    is (* ~ '!')('hi'), 'hi!', 'a WhateverCode called as a value';
    is (* + 1)(1|2).^name, 'Junction', 'a Junction argument autothreads';
    is (* == 1)(any(1, 2)).^name, 'Junction', 'a comparison over a Junction';
    my $off = 10;
    my &g = * + $off;
    is g(1), 11, 'a WhateverCode reads its capture';
    $off = 20;
    is g(1), 21, 'a later write to the captured variable is seen';
    is (* + *)(3, 4), 7, 'a two-star WhateverCode';
    sub wc-in-sub() { my &w = * + 1; w(1) }
    is wc-in-sub(), 2, 'a WhateverCode created inside a sub';
    throws-like { (* + 1)('abc') }, X::Str::Numeric, 'a failing coercion in the body dies';
    my @t;
    for 1, 2 { @t.push((* + 10)($_)) }
    is @t.join(','), '11,12', 'called with the loop topic as its argument';
    $_ = 5;
    is (* + 1)(1), 2, 'the argument, not the caller topic, is the star';
}

# A placeholder block (#13286).
{
    my &p = { $^a + $^b };
    is p(1, 2), 3, 'a placeholder block called by name';
    is { $^b - $^a }(1, 5), 4, 'placeholders bind in sorted-name order';
    is (3, 1, 2).sort({ $^b <=> $^a }).join(','), '3,2,1', 'a placeholder block as a sort comparator';
    is (1..5).reduce({ $^a + $^b }), 15, 'a placeholder block as a reduce callback';
    my @a = 1, 2, 3;
    is { $^x }(@a).raku, '$[1, 2, 3]', 'a placeholder is an item binding';
    throws-like { p(1) }, Exception, 'too few arguments dies';
    throws-like { p(1, 2, 3) }, Exception, 'too many arguments dies';
    is { $^a + 1 }(1|2).^name, 'Junction', 'a Junction argument';
    my @F = (* + 1, { $^a * $^b }, * - *);
    is "{@F[0](1)} {@F[1](2, 3)} {@F[2](10, 4)}", '2 6 6', 'a list of WhateverCode and placeholder blocks';
}

# Variable arguments: a plain parameter takes the value, an `is raw` one the
# variable (#13286).
{
    my Int $x = 3;
    my $y = 4;
    is (-> $a, $b { $a + $b })($x, $y), 7, 'variable arguments to a two-parameter block';
    is { $^a * $^b }($x, $y), 12, 'variable arguments to a placeholder block';
    is (* - *)($y, $x), 1, 'variable arguments to a two-star WhateverCode';
    my $s = 0;
    my $f = -> $a, $b { $a + $b };
    $s = $f($s, 1) for ^10;
    is $s, 10, 'the accumulator passed back in';
}
