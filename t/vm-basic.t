use Test;
plan 123;

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

# --- Phase 5: newly compiled constructs ---

# Prefix + (numeric coercion, compiled NumCoerce)
is +"42", 42, 'prefix + on string coerces to Int';
is +True, 1, 'prefix + on True returns 1';

# Prefix ~ (string coercion, compiled StrCoerce)
is ~42, "42", 'prefix ~ on int coerces to Str';
is ~True, "True", 'prefix ~ on True coerces to Str';

# Prefix ^ (upto range, compiled UptoRange)
my $upto_sum = 0;
for ^5 -> $n { $upto_sum = $upto_sum + $n; }
is $upto_sum, 10, 'prefix ^ creates 0..^n range';

# so keyword (boolean coercion, compiled BoolCoerce)
is so 1, True, 'so keyword coerces truthy to True';
is so 0, False, 'so keyword coerces falsy to False';

# Prefix ++ (compiled PreIncrement)
my $pre_inc = 5;
my $pre_inc_val = ++$pre_inc;
is $pre_inc_val, 6, 'prefix ++ returns new value';
is $pre_inc, 6, 'prefix ++ increments variable';

# Prefix -- (compiled PreDecrement)
my $pre_dec = 10;
my $pre_dec_val = --$pre_dec;
is $pre_dec_val, 9, 'prefix -- returns new value';
is $pre_dec, 9, 'prefix -- decrements variable';

# given/when (compiled Given/When)
my $gw_result = "";
given 42 {
    when 10 { $gw_result = "ten" }
    when 42 { $gw_result = "forty-two" }
    when 99 { $gw_result = "ninety-nine" }
}
is $gw_result, "forty-two", 'given/when integer matching';

# given/when/default (compiled Default)
my $gd_result = "";
given "unknown" {
    when "hello" { $gd_result = "greeting" }
    default { $gd_result = "other" }
}
is $gd_result, "other", 'given/when/default with default branch';

# repeat while loop (compiled RepeatLoop)
my $rw_count = 0;
repeat {
    $rw_count = $rw_count + 1;
} while $rw_count < 3;
is $rw_count, 3, 'repeat while loop';

# repeat while runs at least once
my $rw_once = 0;
repeat {
    $rw_once = $rw_once + 1;
} while False;
is $rw_once, 1, 'repeat while runs body at least once';

# := bind assignment (compiled same as =)
my $bind_val := 42;
is $bind_val, 42, ':= bind assignment works';

# --- Phase 6: remaining binary ops, proceed/succeed, match-assign ---

# Smart match (compiled SmartMatch/NotMatch)
is (42 ~~ 42), True, 'smart match integer equality';
is (42 !~~ 99), True, 'not-match integer inequality';

# Three-way comparison (compiled Spaceship/Cmp/Leg)
is (1 <=> 2), Less, 'spaceship less';
is (2 <=> 2), Same, 'spaceship same';
is (3 <=> 2), More, 'spaceship more';
is (1 cmp 2), Less, 'cmp less';
is ("a" leg "b"), Less, 'leg string less';

# Identity/value equality (compiled StrictEq/Eqv)
is (42 === 42), True, 'strict identity eq';
is (42 eqv 42), True, 'eqv value eq';

# Divisibility (compiled DivisibleBy)
is (10 %% 5), True, '%% divisible';
is (10 %% 3), False, '%% not divisible';

# Keyword math (compiled IntDiv/IntMod/Gcd/Lcm)
is (7 div 2), 3, 'div integer division';
is (7 mod 3), 1, 'mod keyword';
is (12 gcd 8), 4, 'gcd';
is (4 lcm 6), 12, 'lcm';

# Repetition (compiled StringRepeat/ListRepeat)
is ("ab" x 3), "ababab", 'string repeat x';

# Bitwise (compiled BitAnd/BitOr/BitXor/BitShiftLeft/BitShiftRight)
is (0b1100 +& 0b1010), 8, 'bitwise and';
is (0b1100 +| 0b1010), 14, 'bitwise or';
is (1 +< 3), 8, 'bitwise shift left';
is (16 +> 2), 4, 'bitwise shift right';

# Proceed/Succeed (compiled opcodes)
my $ps_result = "";
given 42 {
    when 42 { $ps_result = "first"; proceed }
    when 42 { $ps_result = $ps_result ~ "+second" }
}
is $ps_result, "first+second", 'proceed continues to next when';

# MatchAssign (=~ compiled with StrCoerce)
my $ma_val = 42;
$ma_val =~ "hello";
is $ma_val, "hello", 'match-assign coerces to string';

# --- Phase 7: newly compiled expressions ---

# EnvIndex (%*ENV<key> compiled to GetEnvIndex)
my $env_path = %*ENV<PATH>;
ok $env_path.defined, '%*ENV<PATH> is defined (compiled to GetEnvIndex)';
ok $env_path.chars > 0, '%*ENV<PATH> has content';

# Exists on env var (compiled to ExistsEnvIndex)
my $env_exists = %*ENV<PATH>:exists;
is $env_exists, True, '%*ENV<PATH>:exists returns True';

# Exists on non-existent env var
my $env_noexist = %*ENV<MUTSU_TEST_NONEXISTENT_VAR_XYZ>:exists;
is $env_noexist, False, 'non-existent env var :exists returns False';

# Reduction ([+] compiled to Reduction opcode)
my @rlist = (1, 2, 3, 4, 5);
is ([+] @rlist), 15, '[+] reduction on array';
is ([*] @rlist), 120, '[*] reduction on array';
is ([~] <a b c>), "abc", '[~] reduction on string list';

# Reduction on empty list returns identity
is ([+] ()), 0, '[+] on empty list returns 0';
is ([*] ()), 1, '[*] on empty list returns 1';
is ([~] ()), "", '[~] on empty list returns ""';
