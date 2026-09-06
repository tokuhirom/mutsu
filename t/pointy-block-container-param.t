use Test;

# A pointy block's `@`/`%` parameter binds the CALLER'S container, so `.push`,
# `.unshift`, `.shift` and element assignment inside the block are visible to
# the caller -- exactly like a named sub's `@`/`%` parameter.
#
# mutsu used to lose this for a block with EXACTLY ONE parameter: the parser
# routed such a block through `Expr::Lambda`, which keeps only a sigil-STRIPPED
# name and no `ParamDef` at all (`-> @x` and `-> $x` both became `param: "x"`),
# so it compiled to `MakeLambda` and its argument went through the defs-less
# legacy binder, which binds by value. Two-parameter blocks were already
# correct, because they take the `AnonSubParams` path.

plan 18;

{
    my $push = -> @stack { @stack.unshift('for') };
    my @s;
    $push(@s);
    is @s.raku, '["for"]', 'single-parameter pointy block mutates the caller array';
    $push(@s);
    is @s.raku, '["for", "for"]', 'and again on a second call';
}

{
    my $pop = -> @stack { @stack.shift // 'EMPTY' };
    my @s = 'a', 'b';
    is $pop(@s), 'a', 'shift through a one-parameter block returns the element';
    is @s.raku, '["b"]', 'and removes it from the caller array';
}

{
    my $set = -> %h { %h<z> = 1 };
    my %p;
    $set(%p);
    is %p.raku, '{:z(1)}', 'single-parameter pointy block mutates the caller hash';
}

{
    my $elem = -> @a { @a[0] = 'X' };
    my @s = 'a', 'b';
    $elem(@s);
    is @s.raku, '["X", "b"]', 'element assignment through the parameter propagates';
}

# The same block reached through every callable-holding shape.
{
    my &cv = -> @x { @x.unshift('C') };
    my %h  = k => -> @x { @x.unshift('H') };
    my @f  = (-> @x { @x.unshift('R') },);

    my @a1; cv(@a1);      is @a1.raku, '["C"]', 'through a bare &-sigil variable';
    my @a2; &cv(@a2);     is @a2.raku, '["C"]', 'through an explicit &-sigil call';
    my @a3; %h<k>(@a3);   is @a3.raku, '["H"]', 'through a hash element';
    my @a4; @f[0](@a4);   is @a4.raku, '["R"]', 'through an array element';
}

# Relayed through a sub, and repeated -- the shape the Template6 parser uses
# (a dispatch table of handlers that share one control stack).
{
    my %handlers =
        push => -> @stack, $a { @stack.unshift('for'); 'pushed' },
        pop  => -> @stack { "popped({@stack.shift // 'EMPTY'})" };

    sub act(@stack, *@stmts) {
        my $name = @stmts.shift;
        %handlers{$name}(@stack, |@stmts)
    }
    sub run() {
        my @s;
        my $a = act(@s, 'push', 'x');
        my $b = act(@s, 'pop');
        "$a/$b"
    }
    is run(), 'pushed/popped(for)', 'slurpy relay, first call';
    is run(), 'pushed/popped(for)', 'slurpy relay, second call';
    is run(), 'pushed/popped(for)', 'slurpy relay, third call';
}

# Routing the one-parameter form through the full signature path must not
# change anything else about the block.
{
    my $a = -> @x { @x.elems };
    is $a.WHAT.^name, 'Block', 'a one-parameter pointy block is still a Block';
    is $a.arity, 1, 'arity';
    is $a.signature.gist, '(@x)', 'the signature keeps the sigil';
    is $a([1, 2, 3]), 3, 'it still binds a plain Positional argument';
    dies-ok { $a(42) }, 'and now rejects a non-Positional argument';
}
