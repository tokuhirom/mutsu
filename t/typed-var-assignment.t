use Test;
plan 17;

# Scalar typed variable assignment
{
    my Int $x = 42;
    is $x, 42, "Int typed variable initial assignment";

    $x = 99;
    is $x, 99, "Int typed variable reassignment with valid type";

    dies-ok { $x = "hello" }, "Int typed variable rejects Str assignment";
    dies-ok { my $n = 3.14e0; $x = $n }, "Int typed variable rejects Num assignment";
}

# Scalar typed variable with Str
{
    my Str $s = "hello";
    is $s, "hello", "Str typed variable initial assignment";

    $s = "world";
    is $s, "world", "Str typed variable reassignment with valid type";

    dies-ok { $s = 42 }, "Str typed variable rejects Int assignment";
}

# Typed array: push
{
    my Int @a = 1, 2, 3;
    is @a.elems, 3, "Int typed array initial assignment";

    @a.push(4);
    is @a.elems, 4, "Int typed array push with valid type";

    dies-ok { @a.push("bad") }, "Int typed array push rejects Str";
}

# Typed array: unshift
{
    my Int @b = 10, 20;
    @b.unshift(5);
    is @b[0], 5, "Int typed array unshift with valid type";

    dies-ok { @b.unshift("bad") }, "Int typed array unshift rejects Str";
}

# Typed array: append
{
    my Int @c = 1;
    @c.append(2, 3);
    is @c.elems, 3, "Int typed array append with valid types";

    dies-ok { @c.append("bad") }, "Int typed array append rejects Str";
}

# Typed hash
{
    my Str %h;
    %h<a> = "ok";
    is %h<a>, "ok", "Str typed hash valid assignment";

    dies-ok { %h<b> = 42 }, "Str typed hash rejects Int value";
}

# Error message format
{
    my Int $x = 42;
    try { $x = "hello"; CATCH { default { is .message, "X::TypeCheck::Assignment: Type check failed in assignment to \$x; expected Int but got Str (\"hello\")", "error message matches expected format" } } }
}
