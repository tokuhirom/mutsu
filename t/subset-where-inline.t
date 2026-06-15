use Test;

# Pins the subset `where` predicate evaluation paths after the compile-once
# cache + inline-body fast path (CP-3 Track 2): a `$_`-only predicate (bare
# block or Whatever-currying) runs its body inline with `$_` bound, while
# params/placeholders/bare-expr fall through to the closure/smartmatch path.
# All shapes must remain behavior-identical.

plan 25;

# --- inline block ($_-only bare block) ---
subset Even of Int where { $_ %% 2 };
ok 4 ~~ Even, 'inline block: 4 is Even';
nok 5 ~~ Even, 'inline block: 5 is not Even';
ok 0 ~~ Even, 'inline block: 0 is Even';

# --- Whatever-currying (desugars to Lambda{param:"_"}) ---
subset Small of Int where * < 1000;
ok 5 ~~ Small, 'whatever: 5 is Small';
nok 5000 ~~ Small, 'whatever: 5000 is not Small';

# --- multi-statement block with my (inline path, scoped my) ---
subset Halvable of Int where { my $h = $_ div 2; $h * 2 == $_ };
ok 10 ~~ Halvable, 'multi-stmt block: 10 halvable';
nok 11 ~~ Halvable, 'multi-stmt block: 11 not halvable';

# --- named pointy param (inline path: Lambda{param}) ---
subset Pos of Int where -> $x { $x > 0 };
ok 3 ~~ Pos, 'pointy param: 3 is Pos';
nok -3 ~~ Pos, 'pointy param: -3 is not Pos';

# --- named pointy param closing over an outer lexical ---
my $floor = 100;
subset Above of Int where -> $n { $n > $floor };
ok 150 ~~ Above, 'pointy param closure: 150 > 100';
nok 50 ~~ Above, 'pointy param closure: 50 not > 100';

# --- placeholder block (closure path) ---
subset Big of Int where { $^a > 100 };
ok 200 ~~ Big, 'placeholder block: 200 is Big';
nok 50 ~~ Big, 'placeholder block: 50 is not Big';

# --- bare-expr type predicate (smartmatch path) ---
subset MyInt where Int;
ok 7 ~~ MyInt, 'bare-expr type: 7 is MyInt';
nok "x" ~~ MyInt, 'bare-expr type: "x" is not MyInt';

# --- bare-expr regex predicate (smartmatch path) ---
subset Digits of Str where /^ \d+ $/;
ok "123" ~~ Digits, 'bare-expr regex: matches';
nok "ab" ~~ Digits, 'bare-expr regex: no match';

# --- closure over outer lexical, including mutation between checks ---
my $limit = 10;
subset Bounded of Int where { $_ < $limit };
ok 5 ~~ Bounded, 'closure over lexical: 5 < 10';
nok 15 ~~ Bounded, 'closure over lexical: 15 not < 10';
$limit = 20;
ok 15 ~~ Bounded, 'closure over lexical: sees mutated $limit (15 < 20)';

# --- subset in signature ---
sub takes-even(Even $e) { $e * 10 }
is takes-even(8), 80, 'subset in signature: accepts 8';
dies-ok { takes-even(7) }, 'subset in signature: rejects 7';

# --- typed variable declaration enforces subset ---
my Even $g = 6;
is $g, 6, 'typed var decl: accepts Even';
dies-ok { my Even $b = 3 }, 'typed var decl: rejects non-Even';

# --- repeated checks (exercises the predicate cache) ---
my $hits = 0;
$hits++ for (2, 4, 6, 8).grep(Even);
is $hits, 4, 'cached predicate: repeated checks all pass';
