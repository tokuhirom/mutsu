use Test;

# A `with` statement may chain *multiple* `orwith` clauses (like several
# `elsif`s) before an optional `else`. Previously only a single `orwith` was
# parsed, so a second `orwith` errored with "Undeclared routine: orwith".

plan 8;

# the documented example: first defined index wins
my $s = "abc";
my $found;
with   $s.index("x") { $found = "a at $_" }
orwith $s.index("b") { $found = "b at $_" }
orwith $s.index("c") { $found = "c at $_" }
else                 { $found = "none" }
is $found, "b at 1", 'first defined orwith clause wins in a 3-clause chain';

# all Nil => else runs
my $r;
with Nil { $r = 1 } orwith Nil { $r = 2 } orwith Nil { $r = 3 } else { $r = "all nil" }
is $r, "all nil", 'else runs when with and every orwith are undefined';

# the topic ($_) is the matching clause's value
my $topic;
with Nil { $topic = "with:$_" }
orwith Nil { $topic = "first:$_" }
orwith 42 { $topic = "second:$_" }
is $topic, "second:42", 'the topic is the matching orwith clause value';

# a single orwith still works (regression)
my $one;
with Nil { $one = "w" } orwith 5 { $one = "o:$_" }
is $one, "o:5", 'a single orwith still works';

# with + orwith, no else, none defined => nothing runs
my $ran = "start";
with Nil { $ran = "w" } orwith Nil { $ran = "o" }
is $ran, "start", 'no clause runs and no error when none is defined and no else';

# the with clause itself wins over later orwiths
my $w;
with 10 { $w = "w:$_" } orwith 20 { $w = "o:$_" }
is $w, "w:10", 'the leading with clause wins when defined';

# a pointy param on an orwith clause
my $p;
with Nil { } orwith 7 -> $x { $p = $x * 2 }
is $p, 14, 'a pointy param binds the orwith clause value';

# an `else -> $pos` after orwith binds $pos to the last orwith value (the last
# tested value), not to the orwith clause's .defined() Bool. (roast S04/with.t)
{
    my $foo = 42;
    with Int -> $p { $foo = "w" } orwith Str -> $p { $foo = "o" } else -> $p { $foo = $p }
    is $foo.^name, "Str", 'else -> $pos binds to the last orwith value, not a Bool';
}
