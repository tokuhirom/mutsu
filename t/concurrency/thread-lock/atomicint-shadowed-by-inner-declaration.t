use Test;

plan 25;

# An atomic scalar's target is the binding the call site reaches lexically, not
# the bare name. An inner `my atomicint $y` that shadows an outer `$y` is its
# own variable, so every `⚛` operator on it must leave the outer one alone and
# the outer one must carry on unharmed after the block (#12006).

# The reported shape: post-increment, then a plain read of each binding.
{
    my atomicint $y = 1;
    my @seen;
    {
        my atomicint $y = 20;
        @seen.push: $y⚛++;
        @seen.push: $y;
    }
    @seen.push: $y;
    is @seen, [20, 21, 1], '$y⚛++ in a shadowing block hits the inner binding';
    is $y, 1, 'the outer atomicint is untouched after the block';
}

# The outer binding is still a working atomic after the shadow is gone.
{
    my atomicint $y = 1;
    {
        my atomicint $y = 20;
        $y⚛++;
    }
    is $y⚛++, 1, 'the outer $y⚛++ after the block starts from the outer value';
    is $y, 2, 'and the outer variable holds its own increment';
}

# An outer variable of any type: only the inner declaration is native.
{
    my $y = 1;
    my @seen;
    {
        my atomicint $y = 20;
        @seen.push: $y⚛++;
        @seen.push: $y;
    }
    @seen.push: $y;
    is @seen, [20, 21, 1], 'a plain outer $y does not answer for the inner atomicint';
}

# Every operator, each on the inner binding while the outer one has its own value.
{
    my atomicint $y = 100;
    my @seen;
    {
        my atomicint $y = 20;
        @seen.push: ++⚛$y;
        @seen.push: $y⚛--;
        @seen.push: --⚛$y;
        $y ⚛+= 5;
        @seen.push: $y;
        $y ⚛-= 2;
        @seen.push: $y;
        @seen.push: atomic-fetch-add($y, 10);
        @seen.push: atomic-add-fetch($y, 10);
        @seen.push: atomic-fetch-sub($y, 1);
        @seen.push: atomic-sub-fetch($y, 1);
        @seen.push: atomic-fetch-inc($y);
        @seen.push: atomic-inc-fetch($y);
        @seen.push: atomic-fetch-dec($y);
        @seen.push: atomic-dec-fetch($y);
    }
    is @seen, [21, 21, 19, 24, 22, 22, 42, 42, 40, 40, 42, 42, 40],
        'every integer atomic routine addresses the inner binding';
    is $y, 100, 'the outer variable saw none of them';
}

# The plain-form atomics address the inner binding too.
{
    my atomicint $y = 100;
    my @seen;
    {
        my atomicint $y = 7;
        @seen.push: ⚛$y;
        $y ⚛= 9;
        @seen.push: ⚛$y;
        @seen.push: atomic-fetch($y);
        @seen.push: atomic-assign($y, 11);
        @seen.push: cas($y, 11, 12);
        @seen.push: $y;
        @seen.push: cas($y, 12, 13);
        @seen.push: cas($y, { $_ + 1 });
        @seen.push: $y;
    }
    is @seen, [7, 9, 9, 11, 11, 12, 12, 14, 14],
        '⚛ loads and stores, atomic-fetch/-assign and cas address the inner binding';
    is ⚛$y, 100, 'the outer variable is still 100';
    is $y, 100, 'a plain read of the outer variable agrees';
}

# The same shadow, with the outer binding bumped on each side of the block.
{
    my atomicint $y = 1;
    $y⚛++;
    my @seen;
    {
        my atomicint $y = 50;
        $y⚛++;
        @seen.push: $y;
    }
    $y⚛++;
    @seen.push: $y;
    is @seen, [51, 3], 'outer, inner and outer again each keep their own count';
}

# A shadow per loop iteration is a fresh binding each time.
{
    my atomicint $y = 1000;
    my @seen;
    for 1..3 -> $i {
        my atomicint $y = $i * 10;
        $y⚛++;
        @seen.push: $y;
    }
    is @seen, [11, 21, 31], 'a shadow declared in a loop body is fresh every iteration';
    is $y, 1000, 'the outer variable does not see the loop body';
}

# Two levels of shadowing.
{
    my atomicint $y = 1;
    my @seen;
    {
        my atomicint $y = 20;
        {
            my atomicint $y = 300;
            @seen.push: $y⚛++;
            @seen.push: $y;
        }
        @seen.push: $y⚛++;
        @seen.push: $y;
    }
    @seen.push: $y⚛++;
    @seen.push: $y;
    is @seen, [300, 301, 20, 21, 1, 2], 'each nesting level addresses its own binding';
}

# A shadow inside a routine and inside a closure.
{
    my atomicint $y = 1;
    sub shadowing() {
        my atomicint $y = 20;
        $y⚛++;
        $y
    }
    is shadowing(), 21, 'a routine-local atomicint shadows the file-scope one';
    is $y, 1, 'the file-scope variable is untouched';

    my &c = -> {
        my atomicint $y = 30;
        $y⚛++;
        $y
    };
    is c(), 31, 'a closure-local atomicint shadows the enclosing one';
    is $y, 1, 'the enclosing variable is untouched';
}

# The shadowing binding is itself shared with the threads it spawns.
{
    my atomicint $y = 1;
    my $inner;
    {
        my atomicint $y = 0;
        await (^4).map: { start { $y⚛++ for ^100 } };
        $inner = $y;
    }
    is $inner, 400, 'worker threads increment the inner binding';
    is $y, 1, 'and the outer one is left alone';
}

# A thread that bumps the outer variable while a shadow is live in this scope.
{
    my atomicint $y = 0;
    {
        my atomicint $y = 5;
        $y⚛++;
        is $y, 6, 'the shadow counts on its own inside the block';
    }
    await (^4).map: { start { $y⚛++ for ^100 } };
    is $y, 400, 'the outer variable counts the threads started after the block';
}

# An untyped outer variable read by name inside the shadowing block's closure.
{
    my $z = 5;
    my atomicint $y = 1;
    {
        my atomicint $y = 10;
        my &bump = { $y⚛++ };
        bump();
        bump();
        is $y, 12, 'a closure inside the shadow bumps the shadowing binding';
    }
    is $y, 1, 'the outer variable is untouched by it';
    is $z, 5, 'and so is an unrelated variable';
}
