use Test;
plan 75;

# Literal compilation
is 42, 42, 'integer literal';
is 3.14, 3.14, 'float literal';
is "hello", "hello", 'string literal';
is True, True, 'True literal';
is False, False, 'False literal';

# Variable declaration and access (compiled VarDecl + GetGlobal)
my $x = 10;
is $x, 10, 'variable declaration and access';

# Arithmetic (compiled binary ops)
is 2 + 3, 5, 'addition';
is 10 - 4, 6, 'subtraction';
is 3 * 7, 21, 'multiplication';
is 10 / 4, 2.5, 'division';
is 17 % 5, 2, 'modulo';
is 2 ** 8, 256, 'power';

# Unary (compiled)
is -5, -5, 'unary negate';
is !True, False, 'unary not';
is ?1, True, 'bool coerce';

# String concat (compiled)
is "foo" ~ "bar", "foobar", 'string concatenation';

# Numeric comparison (compiled)
is 3 == 3, True, 'numeric equality';
is 3 != 4, True, 'numeric inequality';
is 2 < 5, True, 'less than';
is 5 > 2, True, 'greater than';

# String comparison (compiled)
is "abc" eq "abc", True, 'string eq';
is "abc" ne "def", True, 'string ne';

# Short-circuit operators (compiled)
is (1 && 2), 2, '&& truthy both';
is (0 || 5), 5, '|| first falsy';
my $nil_val = Nil;
is ($nil_val // 42), 42, '// with nil left';
is (7 // 42), 7, '// with defined left';

# Ternary (compiled)
is (True ?? "yes" !! "no"), "yes", 'ternary true';
is (False ?? "yes" !! "no"), "no", 'ternary false';

# If statement (compiled)
my $result = "";
if True { $result = "then" } else { $result = "else" }
is $result, "then", 'if-then compiled';

# Array literal (compiled)
my @arr = (1, 2, 3);
is @arr.elems, 3, 'array literal compiled';

# While loop (compiled WhileLoop opcode)
my $w = 0;
my $wsum = 0;
while $w < 5 { $wsum = $wsum + $w; $w = $w + 1; }
is $wsum, 10, 'while loop compiled';

# For loop (compiled ForLoop opcode)
my $fsum = 0;
for 1..4 -> $n { $fsum = $fsum + $n; }
is $fsum, 10, 'for loop compiled';

# For loop with $_ default
my $usum = 0;
for 1..3 { $usum = $usum + $_; }
is $usum, 6, 'for loop with $_ compiled';

# Block compilation (phaser-free)
my $bval = 0;
{ $bval = 42; }
is $bval, 42, 'block compiled inline';

# Simple assignment (compiled)
my $aval = 1;
$aval = 99;
is $aval, 99, 'simple assign compiled';

# Loop control: last
my $lsum = 0;
for 1..5 -> $n { last if $n == 4; $lsum = $lsum + $n; }
is $lsum, 6, 'last in for loop';

# Loop control: next
my $nsum = 0;
for 1..5 -> $n { next if $n == 3; $nsum = $nsum + $n; }
is $nsum, 12, 'next in for loop';

# Labeled loop: next LABEL
my $lr = '';
OUTER: for 1..2 -> $i {
    for 1..2 -> $j {
        next OUTER if $j == 2;
        $lr = $lr ~ "$i.$j ";
    }
}
is $lr, '1.1 2.1 ', 'next LABEL in compiled for loop';

# Labeled loop: last LABEL
my $lr2 = '';
DONE: for 1..3 -> $i {
    for 1..3 -> $j {
        last DONE if $i == 2;
        $lr2 = $lr2 ~ "$i.$j ";
    }
}
is $lr2, '1.1 1.2 1.3 ', 'last LABEL in compiled for loop';

# --- Phase 3: newly compiled constructs ---

# Expression-level function call (compiled CallFunc)
sub add($a, $b) { $a + $b }
is add(3, 4), 7, 'function call compiled to CallFunc';

# Method call on non-variable target (compiled CallMethod)
is (1, 2, 3).elems, 3, 'method call on literal compiled to CallMethod';
is "hello".chars, 5, 'method call on string literal compiled';

# Statement-level call (compiled ExecCall) -- is/ok themselves use ExecCall
ok 1 == 1, 'statement call compiled to ExecCall';

# Indexing (compiled Index)
my @idx = (10, 20, 30);
is @idx[1], 20, 'array indexing compiled to Index';

# String interpolation (compiled StringConcat)
my $who = "world";
is "hello $who", "hello world", 'string interpolation compiled to StringConcat';

# Postfix ++ (compiled PostIncrement)
my $pi = 5;
my $old_pi = $pi++;
is $old_pi, 5, 'postfix ++ returns old value';
is $pi, 6, 'postfix ++ increments variable';

# Postfix -- (compiled PostDecrement)
my $pd = 10;
my $old_pd = $pd--;
is $old_pd, 10, 'postfix -- returns old value';
is $pd, 9, 'postfix -- decrements variable';

# C-style loop (compiled CStyleLoop)
my $csum = 0;
loop (my $ci = 1; $ci <= 5; $ci++) { $csum = $csum + $ci; }
is $csum, 15, 'C-style loop compiled to CStyleLoop';

# C-style loop with last
my $clsum = 0;
loop (my $cl = 1; $cl <= 10; $cl++) { last if $cl > 3; $clsum = $clsum + $cl; }
is $clsum, 6, 'C-style loop with last';

# C-style loop with next
my $cnsum = 0;
loop (my $cn = 1; $cn <= 5; $cn++) { next if $cn == 3; $cnsum = $cnsum + $cn; }
is $cnsum, 12, 'C-style loop with next';

# Hash literal (compiled MakeHash)
my %h = (a => 1, b => 2);
is %h<a>, 1, 'hash literal compiled to MakeHash';
is %h<b>, 2, 'hash literal second key';

# Assignment as expression (compiled AssignExpr)
my $ae;
my $ae_val = ($ae = 42);
is $ae, 42, 'assign expr sets variable';
is $ae_val, 42, 'assign expr returns value';

# Loop control: last/next/redo as opcodes
my $redo_count = 0;
my $redo_done = 0;
for 1..1 {
    $redo_count = $redo_count + 1;
    if $redo_count < 3 && !$redo_done {
        redo if $redo_count < 3;
    }
    $redo_done = 1;
}
is $redo_count, 3, 'redo as compiled opcode';

# Negate on string-numeric values (Inf, NaN)
is -Inf, -Inf, 'negate Inf bareword';

# --- Phase 4: newly compiled constructs ---

# Array variable access (compiled GetArrayVar)
my @va = (10, 20, 30);
is @va.elems, 3, 'array variable compiled to GetArrayVar';

# Hash variable access (compiled GetHashVar)
my %vh = (x => 1, y => 2);
is %vh<x>, 1, 'hash variable compiled to GetHashVar';

# Bareword compilation (compiled GetBareWord)
is True, True, 'bareword True compiled to GetBareWord';

# Range operators (compiled MakeRange, MakeRangeExcl, etc.)
my $rsum1 = 0;
for 1..5 -> $n { $rsum1 = $rsum1 + $n; }
is $rsum1, 15, 'range 1..5 compiled to MakeRange';
my $rsum2 = 0;
for 1..^5 -> $n { $rsum2 = $rsum2 + $n; }
is $rsum2, 10, 'range 1..^5 compiled to MakeRangeExcl';

# String comparison (compiled StrLt, StrGt, StrLe, StrGe)
is "a" lt "b", True, 'string lt compiled';
is "b" gt "a", True, 'string gt compiled';
is "a" le "a", True, 'string le compiled';
is "b" ge "a", True, 'string ge compiled';
is "b" lt "a", False, 'string lt false compiled';
is "a" gt "b", False, 'string gt false compiled';

# Method call on $var target (compiled CallMethodMut)
my $absval = -42;
is $absval.abs, 42, 'method call on $var compiled to CallMethodMut';

# Method call on @arr target (compiled CallMethodMut with writeback)
my @parr = (1, 2, 3);
@parr.push(4);
is @parr.elems, 4, 'push on @array compiled to CallMethodMut';
is @parr[3], 4, 'push result correct';

# Method call on @arr - non-mutating (compiled CallMethodMut)
my @jarr = (1, 2, 3);
is @jarr.join(","), "1,2,3", 'join on @array compiled to CallMethodMut';

# Die statement (compiled Die)
my $died = False;
try { die "oops"; CATCH { default { $died = True } } }
is $died, True, 'die compiled to Die opcode';

# Method call on literal target still works (compiled CallMethod)
is "HELLO".lc, "hello", 'method call on literal still uses CallMethod';
