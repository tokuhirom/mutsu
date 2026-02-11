use Test;
plan 39;

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
