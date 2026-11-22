use Test;

# A multi candidate typed with a subset whose predicate is one integer
# comparison (`subset Small of Int where * < 100`), or with `UInt`, is checked
# per call by that comparison alone, decided once per argument-type key
# (ADR-18580 D1). Every call shape below must give rakudo's answer, including
# the ones that fall back to the full check: a big integer, a `Bool`, a subset
# of a subset, and a predicate over a variable. Each list runs twice so the
# second pass uses the cached dispatch program.
plan 10;

subset Small of Int where * < 100;
multi size(Small $x)                  { 1 }
multi size(Int $x where * < 10_000)   { 2 }
multi size(Int $x)                    { 3 }
for ^2 -> $pass {
    is (5, 500, 50_000, -7, 2**70, -2**70, 99, 100).map({ size($_) }).join(','),
        '1,2,3,1,3,1,1,2', "subset and where candidates (pass $pass)";
}

multi u(UInt $x) { "u" }
multi u(Int $x)  { "i" }
for ^2 -> $pass {
    is (5, 0, -1, True, False, 2**70, -2**70).map({ u($_) }).join(','),
        'u,u,i,u,u,u,i', "UInt candidate (pass $pass)";
}

subset Pos of Int where * > 0;
subset Big of Pos where * > 1000;
multi chain(Big $x) { "big" }
multi chain(Pos $x) { "pos" }
multi chain(Int $x) { "int" }
for ^2 -> $pass {
    is (5, 5000, -5).map({ chain($_) }).join(','), 'pos,big,int',
        "a subset of a subset (pass $pass)";
}

my $n = 10;
subset Lex of Int where * < $n;
multi lex(Lex $x) { "lex" }
multi lex(Int $x) { "int" }
is lex(3), 'lex', 'a predicate over a variable: inside';
is lex(30), 'int', 'a predicate over a variable: outside';
$n = 50;
is lex(30), 'lex', 'the predicate reads the variable on every call';
is lex(3), 'lex', '...and still answers the earlier key';
