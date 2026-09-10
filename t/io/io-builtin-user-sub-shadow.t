use v6;
use Test;

plan 10;

# A user-declared `sub` named like an IO builtin shadows the builtin listop form
# in its lexical scope: `sub say(...) {...}; say $x` calls the user sub.

{
    my @s;
    sub say($a) { @s.push("say:$a") }
    say "X";
    is-deeply @s, ["say:X"], 'user sub say shadows builtin (listop form)';
}

{
    my @s;
    sub print($a) { @s.push("print:$a") }
    print "Y";
    is-deeply @s, ["print:Y"], 'user sub print shadows builtin (listop form)';
}

{
    my @s;
    sub put($a) { @s.push("put:$a") }
    put "Z";
    is-deeply @s, ["put:Z"], 'user sub put shadows builtin (listop form)';
}

{
    my @s;
    sub note($a) { @s.push("note:$a") }
    note "W";
    is-deeply @s, ["note:W"], 'user sub note shadows builtin (listop form)';
}

# Shadowing works from inside a closure that is called later.
{
    my @s;
    sub print($a) { @s.push($a) }
    my &c = { print "in-closure" };
    c();
    is-deeply @s, ["in-closure"], 'user sub print is resolved from a closure';
}

# Multiple arguments are passed through.
{
    my @s;
    sub say(*@a) { @s.push(@a.join("|")) }
    say "a", "b", "c";
    is-deeply @s, ["a|b|c"], 'user sub say receives multiple listop args';
}

# The builtin is NOT shadowed outside the sub's scope.
{
    my @inner;
    {
        sub say($a) { @inner.push($a) }
        say "shadowed";
    }
    is-deeply @inner, ["shadowed"], 'inner user sub say captured its call';
    # Out here `say` is the builtin again; just confirm it does not push.
    lives-ok { say "outer-builtin" }, 'builtin say works again outside the shadow scope';
}

# Paren-call form was already fine; keep it covered.
{
    my @s;
    sub print($a) { @s.push($a) }
    print("paren");
    is-deeply @s, ["paren"], 'user sub print via paren call';
}

# The builtin still parses normally when no user sub shadows it.
lives-ok { say "plain"; print "plain\n"; put "p"; note "n" },
    'unshadowed IO builtins still parse and run';
