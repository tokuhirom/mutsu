use Test;
plan 327;

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

# String coercion via assignment
my $ma_val = 42;
$ma_val = ~"hello";
is $ma_val, "hello", 'string coercion via assignment';

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

# --- Phase 8: compiled statements ---

# Take statement (compiled Take opcode)
my @gathered = gather { take 1; take 2; take 3; };
is @gathered.elems, 3, 'take compiled to Take opcode';
is @gathered[0], 1, 'take first value';
is @gathered[2], 3, 'take third value';

# Sub declaration (compiled via InterpretStmt, but recognized)
sub vm_add($a, $b) { $a + $b }
is vm_add(10, 20), 30, 'sub declaration compiled';

# Class declaration (compiled via InterpretStmt)
class VMTestPoint { has $.x; has $.y; method sum() { $.x + $.y } }
my $vpt = VMTestPoint.new(x => 3, y => 4);
is $vpt.sum, 7, 'class declaration compiled';

# Enum declaration (compiled via InterpretStmt)
enum VMColor <Red Green Blue>;
is Red.key, "Red", 'enum declaration compiled';

# BEGIN phaser (compiled inline)
my $begin_val = 0;
BEGIN { $begin_val = 42; }
is $begin_val, 42, 'BEGIN phaser compiled inline';

# React block (compiled inline)
my $react_val = 0;
react { $react_val = 99; }
is $react_val, 99, 'react block compiled inline';

# --- Phase 9: remaining compiled expressions ---

# HyperOp (compiled sub-expressions + HyperOp opcode)
my @hyper_in = (1, 2, 3);
my @hyper_out = @hyper_in >>+>> 10;
is @hyper_out[0], 11, 'hyper op >>+>> first element';
is @hyper_out[1], 12, 'hyper op >>+>> second element';
is @hyper_out[2], 13, 'hyper op >>+>> third element';

# MetaOp R (reverse)
is (10 R- 3), -7, 'Rminus meta op compiled';

# MetaOp X (cross)
my @xresult = (1, 2) X+ (10, 20);
is @xresult.elems, 4, 'Xplus meta op compiled element count';
is @xresult[0], 11, 'Xplus first result';
is @xresult[3], 22, 'Xplus last result';

# MetaOp Z (zip)
my @zresult = (1, 2, 3) Z+ (10, 20, 30);
is @zresult[0], 11, 'Zplus first result';
is @zresult[2], 33, 'Zplus third result';

# Block as expression value (delegate to interpreter)
my $block = { 42 };
is $block(), 42, 'block as expression value';

# Lambda (delegate to interpreter)
my $lam = -> $x { $x * 2 };
is $lam(5), 10, 'lambda expression value';

# --- Phase 10: Local variable slots + native method dispatch ---

# Local variable read/write (GetLocal/SetLocal)
my $loc_x = 42;
is $loc_x, 42, 'local variable read via GetLocal';

# Local variable assignment
my $loc_y = 10;
$loc_y = 20;
is $loc_y, 20, 'local variable assign via SetLocal';

# For-loop param as local
my $loc_fsum = 0;
for 1..5 -> $n { $loc_fsum = $loc_fsum + $n; }
is $loc_fsum, 15, 'for-loop param as local slot';

# $_ topic still works (not a local)
my $topic_sum = 0;
for 1..3 { $topic_sum = $topic_sum + $_; }
is $topic_sum, 6, '$_ topic variable still works';

# Interaction with interpreter fallback (InterpretStmt syncs locals)
my $sub_loc = 0;
sub get_sub_loc() { $sub_loc }
is get_sub_loc(), 0, 'interpreter fallback reads local via env';

# Increment/decrement on locals
my $inc_loc = 5;
$inc_loc++;
is $inc_loc, 6, 'postfix ++ on local variable';
++$inc_loc;
is $inc_loc, 7, 'prefix ++ on local variable';
$inc_loc--;
is $inc_loc, 6, 'postfix -- on local variable';
--$inc_loc;
is $inc_loc, 5, 'prefix -- on local variable';

# Nested for loops with different params
my $nested_sum = 0;
for 1..3 -> $i {
    for 1..2 -> $j {
        $nested_sum = $nested_sum + $i * $j;
    }
}
is $nested_sum, 18, 'nested for loops with local params';

# AssignExpr on locals
my $ae_loc;
my $ae_loc_val = ($ae_loc = 77);
is $ae_loc, 77, 'assign expr sets local';
is $ae_loc_val, 77, 'assign expr returns value for local';

# Native .defined dispatch
is 42.defined, True, 'native .defined on Int';
is Nil.defined, False, 'native .defined on Nil';
my $def_var = "hello";
is $def_var.defined, True, 'native .defined on local Str';

# Native .elems dispatch
my @ne_arr = (1, 2, 3, 4);
is @ne_arr.elems, 4, 'native .elems on array';
my %ne_hash = (a => 1, b => 2);
is %ne_hash.elems, 2, 'native .elems on hash';

# Native .chars dispatch
is "hello".chars, 5, 'native .chars on string literal';
my $chars_str = "world";
is $chars_str.chars, 5, 'native .chars on local variable';

# Native .abs dispatch
is (-42).abs, 42, 'native .abs on negative Int';
is 42.abs, 42, 'native .abs on positive Int';
my $abs_val = -3.14;
is $abs_val.abs, 3.14, 'native .abs on Num';

# Native .uc/.lc dispatch
is "hello".uc, "HELLO", 'native .uc';
is "HELLO".lc, "hello", 'native .lc';

# Native .Int dispatch
is "42".Int, 42, 'native .Int on string';
is 3.14.Int, 3, 'native .Int on Num';
is True.Int, 1, 'native .Int on True';
is False.Int, 0, 'native .Int on False';

# Native .Bool dispatch
is 42.Bool, True, 'native .Bool on non-zero Int';
is 0.Bool, False, 'native .Bool on zero';
is "".Bool, False, 'native .Bool on empty string';

# Native .Str dispatch
is 42.Str, "42", 'native .Str on Int';
is True.Str, "True", 'native .Str on Bool';

# Native .sign dispatch
is 42.sign, 1, 'native .sign positive';
is (-5).sign, -1, 'native .sign negative';
is 0.sign, 0, 'native .sign zero';

# Native .end dispatch
my @end_arr = (10, 20, 30, 40);
is @end_arr.end, 3, 'native .end on array';

# === VM Phase 11: Compiled function bodies ===

# Compiled function call
sub vm_add($a, $b) { $a + $b }
is vm_add(3, 4), 7, 'compiled function call';

# Implicit return (last expression value)
sub vm_last_val { 42 }
is vm_last_val(), 42, 'compiled function implicit return';

# Explicit return
sub vm_early($n) { return $n * 2; 999 }
is vm_early(5), 10, 'compiled function explicit return';

# Function with default param
sub vm_greet($name = "World") { "Hello, $name" }
is vm_greet(), "Hello, World", 'compiled function default param';
is vm_greet("Raku"), "Hello, Raku", 'compiled function with arg';

# Recursive function
sub vm_fact($n) { $n <= 1 ?? 1 !! $n * vm_fact($n - 1) }
is vm_fact(5), 120, 'compiled recursive function';

# Nested function calls
sub vm_double($n) { $n * 2 }
sub vm_quad($n) { vm_double(vm_double($n)) }
is vm_quad(3), 12, 'compiled nested function calls';

# Function with string operations
sub vm_shout($s) { $s.uc }
is vm_shout("hello"), "HELLO", 'compiled function with method call';

# === Try/Catch ===

# Basic try/catch
my $try_result = try { die "oops"; CATCH { default { "caught" } } };
is $try_result, Nil, 'try/catch catches error';

# try without error
my $try_ok = try { 42 };
is $try_ok, 42, 'try without error returns value';

# try with $! access
try { die "boom"; CATCH { default { is $!, "boom", 'try/catch sets $!' } } };

# === Block inlining ===

# do block
my $do_val = do { 42 };
is $do_val, 42, 'do block returns value';

# Block with multiple statements
my $block_val = do { my $x = 10; my $y = 20; $x + $y };
is $block_val, 30, 'block with multiple stmts returns last value';

# === Additional native methods ===

# .flat
my @nested = (1, (2, 3), (4, 5));
is @nested.flat.elems, 5, 'native .flat flattens array';

# .reverse
my @rev = (1, 2, 3);
is @rev.reverse.join(","), "3,2,1", 'native .reverse on array';
is "abc".reverse, "cba", 'native .reverse on string';

# .unique
my @dup = (1, 2, 2, 3, 1);
is @dup.unique.join(","), "1,2,3", 'native .unique removes duplicates';

# .floor / .ceiling / .round
is 3.7.floor, 3, 'native .floor';
is 3.2.ceiling, 4, 'native .ceiling';
is 3.5.round, 4, 'native .round';

# .sqrt
is 9.sqrt, 3e0, 'native .sqrt';

# .words
is "hello world foo".words.elems, 3, 'native .words';

# .lines
is "a\nb\nc".lines.elems, 3, 'native .lines';

# .trim
is "  hello  ".trim, "hello", 'native .trim';
is "  hello  ".trim-leading, "hello  ", 'native .trim-leading';
is "  hello  ".trim-trailing, "  hello", 'native .trim-trailing';

# .so / .not
is 1.so, True, 'native .so truthy';
is 0.not, True, 'native .not falsy';

# .keys / .values
my %kv = a => 1, b => 2;
is %kv.keys.sort.join(","), "a,b", 'native .keys on hash';
is %kv.values.sort.join(","), "1,2", 'native .values on hash';

# === VM Phase 12: Native binary operations ===

# Rat arithmetic (native in VM)
is 1/3 + 1/6, 0.5, 'Rat addition natively in VM';
is 1/2 - 1/4, 0.25, 'Rat subtraction natively in VM';
is 2/3 * 3/4, 0.5, 'Rat multiplication natively in VM';

# Complex arithmetic (native in VM)
is (1+2i) + (3+4i), 4+6i, 'Complex addition natively in VM';
is (5+3i) - (2+1i), 3+2i, 'Complex subtraction natively in VM';

# Junction threading with == (native in VM)
ok 5 == any(3, 5, 7), 'junction == with any';
ok 5 != all(3, 4, 6), 'junction != with all';

# Junction with string compare (native in VM)
ok "b" eq any("a", "b", "c"), 'junction eq with any';

# Bitwise ops (native in VM)
is (0b1100 +& 0b1010), 8, 'bitwise AND native';
is (0b1100 +| 0b1010), 14, 'bitwise OR native';
is (0b1100 +^ 0b1010), 6, 'bitwise XOR native';

# Integer division/mod (native in VM)
is 7 div 3, 2, 'div native in VM';
is 7 mod 3, 1, 'mod native in VM';

# String repeat (native in VM)
is "ab" x 3, "ababab", 'string repeat x native';

# Spaceship (native in VM)
is (1 <=> 2), Less, 'spaceship Less native';
is (2 <=> 2), Same, 'spaceship Same native';
is (3 <=> 1), More, 'spaceship More native';

# === VM Phase 13: Native Sequence/Index ===

# Sequence operator
is (1, 2, 3 ... 7).join(","), "1,2,3,4,5,6,7", 'sequence 1..7 native';
is (1, 3 ... 9).join(","), "1,3,5,7,9", 'sequence 1,3...9 step 2 native';
is (10, 8 ... 2).join(","), "10,8,6,4,2", 'sequence 10,8...2 step -2 native';
is (1 ... 5).join(","), "1,2,3,4,5", 'sequence single seed native';

# Index operator
my @idx-arr = 10, 20, 30, 40, 50;
is @idx-arr[2], 30, 'array index native';
is @idx-arr[0], 10, 'array index 0 native';
is @idx-arr[4], 50, 'array index last native';
my %idx-hash = x => 42, y => 99;
is %idx-hash<x>, 42, 'hash index native';
is %idx-hash<y>, 99, 'hash index str native';

# === VM Phase 14: Native One-Arg Method Dispatch ===

# Zero-arg: chomp, chop, comb
is "hello\n".chomp, "hello", 'chomp removes trailing newline';
is "hello".chomp, "hello", 'chomp no-op without newline';
is "hello".chop, "hell", 'chop removes last char';
is "abc".comb.join(","), "a,b,c", 'comb splits into chars';

# Zero-arg: gist/raku/perl
is 42.gist, "42", 'gist on Int';
is "hello".gist, "hello", 'gist on Str';
is True.gist, "True", 'gist on Bool';

# Zero-arg: head/tail/first on arrays
is (1,2,3).head, 1, 'head returns first element';
is (1,2,3).tail, 3, 'tail returns last element';
is (1,2,3).first, 1, 'first returns first element';

# One-arg: contains, starts-with, ends-with
is "hello world".contains("world"), True, 'contains finds substring';
is "hello world".contains("xyz"), False, 'contains rejects missing substring';
is "hello".starts-with("hel"), True, 'starts-with matches prefix';
is "hello".starts-with("xyz"), False, 'starts-with rejects non-prefix';
is "hello".ends-with("llo"), True, 'ends-with matches suffix';
is "hello".ends-with("xyz"), False, 'ends-with rejects non-suffix';

# One-arg: index
is "hello world".index("world"), 6, 'index finds substring position';
is "hello".index("xyz"), Nil, 'index returns Nil for missing';

# One-arg: substr
is "hello".substr(2), "llo", 'substr from position';
is "hello".substr(0), "hello", 'substr from 0';

# One-arg: split
is "a,b,c".split(",").join(":"), "a:b:c", 'split by comma then join';

# One-arg: join
is (1,2,3).join("-"), "1-2-3", 'join with separator';

# One-arg: head(n), tail(n)
is (1,2,3,4,5).head(3).join(","), "1,2,3", 'head(n) takes first n elements';
is (1,2,3,4,5).tail(2).join(","), "4,5", 'tail(n) takes last n elements';

# One-arg: base
is 255.base(16), "FF", 'base 16';
is 10.base(2), "1010", 'base 2';
is 511.base(8), "777", 'base 8';

# === VM Phase 15: Native Function Dispatch, Two-Arg Methods & Bridge Elimination ===

# Two-arg method: substr(start, len)
is "hello world".substr(0, 5), "hello", 'substr 2-arg from 0';
is "hello world".substr(6, 5), "world", 'substr 2-arg mid';
is "hello".substr(2, 10), "llo", 'substr 2-arg clamp len';
is "hello".substr(0, 0), "", 'substr 2-arg zero len';

# One-arg methods: rindex, fmt, parse-base
is "hello world hello".rindex("hello"), 12, 'rindex finds last occurrence';
is "hello".rindex("xyz"), Nil, 'rindex returns Nil for missing';
is "hello world".rindex("world"), 6, 'rindex finds substring';
is 42.fmt("%05d"), "00042", 'fmt zero-padded int';
is 3.14.fmt("%.1f"), "3.1", 'fmt float precision';
is "ff".parse-base(16), 255, 'parse-base hex';
is "1010".parse-base(2), 10, 'parse-base binary';

# Function-form: string functions
is abs(-42), 42, 'abs function';
is abs(3.14), 3.14, 'abs function float';
is chars("hello"), 5, 'chars function';
is uc("hello"), "HELLO", 'uc function';
is lc("HELLO"), "hello", 'lc function';
is tc("hello world"), "Hello world", 'tc function';
is chomp("hello\n"), "hello", 'chomp function';
is chop("hello"), "hell", 'chop function';
is trim("  hi  "), "hi", 'trim function';
is flip("abc"), "cba", 'flip function';
is chr(65), "A", 'chr function';
is ord("A"), 65, 'ord function';

# Function-form: math functions
is sqrt(4), 2e0, 'sqrt function';
is floor(3.7), 3, 'floor function';
is ceiling(3.2), 4, 'ceiling function';
is round(3.5), 4, 'round function';
is defined(42), True, 'defined function true';
is defined(Nil), False, 'defined function nil';
is elems([1,2,3]), 3, 'elems function';
is reverse([1,2,3]).join(","), "3,2,1", 'reverse function';

# Function-form: 2-arg functions
is join(",", [1,2,3]), "1,2,3", 'join function';
is index("hello world", "world"), 6, 'index function';
ok sin(0) == 0e0, 'sin function';
ok cos(0) == 1e0, 'cos function';
ok exp(0) == 1e0, 'exp function';

# === VM Phase 16: Remaining Pure Native Dispatch ===

# Zero-arg methods: tclc, succ, pred
is "hELLO".tclc, "Hello", 'tclc titlecase';
is 5.succ, 6, 'succ on Int';
is "a".succ, "b", 'succ on Str';
is 5.pred, 4, 'pred on Int';

# Zero-arg methods: min, max on arrays
is (3,1,2).min, 1, 'min method on array';
is (3,1,2).max, 3, 'max method on array';

# Zero-arg methods: log, exp
ok 1.log == 0e0, 'log method on 1';
ok 0.exp == 1e0, 'exp method on 0';

# Zero-arg method: Rat
is 42.Rat.nude.join("/"), "42/1", 'Rat method on Int';

# One-arg methods: round(scale), log(base)
is 3.456.round(0.01), 3.46, 'round with scale';
is 100.log(10), 2e0, 'log with base';

# Functions: asin, acos, atan
ok asin(0) == 0e0, 'asin function';
ok atan(0) == 0e0, 'atan function';

# Functions: flat, first, min, max
is flat([1,[2,3],4]).join(","), "1,2,3,4", 'flat function';
is first([5,6,7]), 5, 'first function';
is min(3,1,2), 1, 'min function variadic';
is max(3,1,2), 3, 'max function variadic';
is min(10,20), 10, 'min function 2-arg';
is max(10,20), 20, 'max function 2-arg';

# Functions: ords, gist
is ords("AB").join(","), "65,66", 'ords function';
is gist(42), "42", 'gist function';

# Functions: log 2-arg, round 2-arg, substr 3-arg
is log(100, 10), 2e0, 'log function with base';
is round(3.456, 0.01), 3.46, 'round function with scale';
is substr("hello world", 0, 5), "hello", 'substr function 3-arg';
is substr("hello world", 6, 5), "world", 'substr function 3-arg mid';

# Functions: chrs
is chrs(65, 66, 67), "ABC", 'chrs function';

# Method .tclc edge case
is "hello WORLD".tclc, "Hello world", 'tclc lowercases rest';

# Method .min/.max on array
is (10, 5, 20, 1).min, 1, 'min method 4 elements';
is (10, 5, 20, 1).max, 20, 'max method 4 elements';
