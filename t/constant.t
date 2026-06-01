use Test;

plan 17;

# Basic constant declarations
constant $a = 42;
is $a, 42, 'constant $a = 42';

constant $b = "hello";
is $b, "hello", 'constant $b = "hello"';

constant $c = 3.14;
is $c, 3.14, 'constant $c = 3.14';

# Constant with expression
constant $d = 10 + 20;
is $d, 30, 'constant with expression';

# Constant with $*PROGRAM
constant $prog = $*PROGRAM;
ok $prog.chars > 0, 'constant $prog = $*PROGRAM is not empty';
ok $prog ~~ IO::Path, 'constant $prog preserves IO::Path type';

# IO::Path.parent traversal
is ".".IO.parent.Str, "..", '.IO.parent from "."';
is ".".IO.parent(2).Str, "../..", '.IO.parent(2) from "."';
is "foo".IO.parent.Str, ".", 'parent of "foo"';
is "foo".IO.parent(2).Str, "..", 'parent(2) of "foo"';
is "a/b/c".IO.parent(3).Str, ".", 'parent(3) of "a/b/c"';
is "a/b/c".IO.parent(4).Str, "..", 'parent(4) of "a/b/c"';

# A bare constant shadows a same-named sub, but parens always call the sub.
{
    sub foo0 { "OH NOES" };
    constant foo0 = 5;
    is foo0,   5,         'bare constant wins against same-named sub';
    is foo0(), 'OH NOES', 'parens always indicate a sub call';
}

# Constants are `our`-scoped: a constant declared in an inner block is visible
# in the enclosing scope and inside EVAL.
{
    {
        constant foo2 = 42;
    }
    is foo2, 42, 'inner-block constant visible in outer scope';
    ok (EVAL 'foo2 == 42'), 'inner-block constant visible inside EVAL';
}

# A `try` whose expression yields a soft Failure handles it: later numeric use
# returns False (uninitialized) instead of re-throwing the stored exception.
{
    constant @arr = [1, 2, 3];
    constant $oob = try @arr[10];
    ok !($oob == 99), 'try-handled Failure does not re-throw on numeric use';
}
