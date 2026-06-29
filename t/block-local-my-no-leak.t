use Test;

# A `my` variable declared inside an `if`/`while` branch is block-local and
# must not leak into the enclosing scope (Raku lexical scoping). Previously
# such fresh (non-shadowing) declarations leaked because the branch's
# shadow-only restore only re-exposed names that shadowed an outer binding.

plan 8;

# if-branch fresh declaration does not leak out
my $seen-if = False;
if 1 { my $b = 1; $seen-if = $b == 1; }
ok $seen-if, 'my var is usable inside the if branch';
ok !(EVAL 'my $a = 1; if 1 { my $leak_if = 5 }; defined try { $leak_if }'),
    'if-branch my does not leak (try read is undefined/dies)';

# while-branch fresh declaration does not leak out
my $i = 0;
my $seen-while = False;
while $i < 1 { my $w = 7; $seen-while = $w == 7; $i++; }
ok $seen-while, 'my var is usable inside the while body';

# A `my` that *shadows* an outer binding restores the outer value on exit.
my $x = 10;
if 1 { my $x = 20; }
is $x, 10, 'shadowing my in if branch restores outer value on exit';

my $y = 100;
my $j = 0;
while $j < 1 { my $y = 200; $j++; }
is $y, 100, 'shadowing my in while body restores outer value on exit';

# `:=` binding to an *outer* variable inside an if branch must still take effect
# (this is why the branch uses a shadow-only restore, not a full env restore).
my $z = 1;
if 1 { $z := 99; }
is $z, 99, 'bind to outer var inside if branch survives';

# Loop accumulation into an outer container must survive across iterations.
my @acc;
for ^3 { @acc.push($_); }
is @acc.join(','), '0,1,2', 'outer container accumulation survives the loop';

# A `my` declared in a `while` condition is visible after the loop.
my $r;
while my $c = 1 { $r = $c; last; }
is $r, 1, 'my in while condition is seen from the body';
