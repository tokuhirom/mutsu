use Test;

plan 8;

# Basic capture forwarding with parenthesized call
{
    sub inner($x) { $x * 2 }
    sub outer(|c) { inner(|c) }
    is outer(21), 42, "capture forwarding with parens works";
}

# Non-parenthesized capture forwarding (listop style)
{
    sub target($x) { $x + 10 }
    sub wrapper(|c) { target |c }
    is wrapper(5), 15, "capture forwarding without parens works";
}

# Named argument forwarding
{
    sub greet(:$name, :$greeting = "Hello") { "$greeting, $name!" }
    sub wrap-greet(|c) { greet(|c) }
    is wrap-greet(:name("World")), "Hello, World!", "named arg forwarding works";
    is wrap-greet(:name("Alice"), :greeting("Hi")), "Hi, Alice!", "multiple named args forwarded";
}

# Mixed positional and named forwarding
{
    sub mixed($x, :$y = 0) { $x + $y }
    sub wrap-mixed(|c) { mixed(|c) }
    is wrap-mixed(10), 10, "positional only through capture";
    is wrap-mixed(10, :y(5)), 15, "positional + named through capture";
}

# Multi-arg forwarding
{
    sub add($a, $b) { $a + $b }
    sub wrap-add(|c) { add(|c) }
    is wrap-add(3, 4), 7, "multi-arg capture forwarding";
}

# Capture with statement-level call (ExecCallSlip)
{
    sub printer($x) { $x.Str }
    sub wrap-printer(|c) { printer |c }
    is wrap-printer(42), "42", "statement-level capture slip works";
}
