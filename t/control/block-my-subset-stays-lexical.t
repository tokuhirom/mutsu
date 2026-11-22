use Test;

# A `my subset` declared in a block is lexical to the block: after the block
# the outer `subset S` of the same name answers again, whatever kind of block
# it was (#18217).

plan 11;

subset S of Int;

my $f = { my subset S of Str; 1 };
$f();
ok 3 ~~ S, 'after a called closure';

{ my subset S of Str; }
ok 3 ~~ S, 'after a bare block';

(1..2).map({ my subset S of Str; 1 }).eager;
ok 3 ~~ S, 'after a .map callback';

for 1..2 { my subset S of Str; my $x = S; ok 'a' ~~ $x, 'inside a loop body the block subset answers' }
ok 3 ~~ S, 'after a loop body that read the name';

if 1 { my subset S of Str; ok 'b' ~~ S, 'inside an if branch the block subset answers' }
ok 3 ~~ S, 'after an if branch';

{
    my subset S of Str;
    ok 'c' ~~ S, 'inside a bare block the block subset answers';
}

{ my subset T of Int where * > 5; }
nok (try EVAL 'T').defined, 'a block-only subset name is gone after the block';
ok 3 ~~ S, 'and the outer subset still answers at the end';
