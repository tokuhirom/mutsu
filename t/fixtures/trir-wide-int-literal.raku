# An integer literal wider than 32 bits is an `Int`, not a native `int`
# operand, so native-int arithmetic against it promotes instead of wrapping
# (#9234). `t/vm/codegen/adr0110-trir-wide-int-literal.t` runs this with TRIR on and
# off and requires rakudo's transcript from both. Every routine is called
# twice, so a first-run-only answer cannot pass.
my sub plus-max(int $a) { $a + 9223372036854775807 }
my sub plus-wide(int $a) { $a + 2147483648 }
my sub max-plus(int $a) { 9223372036854775807 + $a }
my sub minus-wide(int $a) { $a - 2147483649 }
my sub times-wide(int $a) { $a * 4294967296 }
my sub cmp-wide(int $a) { $a < 4294967296 ?? 'lt' !! 'ge' }

my $max = 9223372036854775807;
for ^2 {
    say 'plus-max=', plus-max(1);
    say 'plus-wide=', plus-wide($max);
    say 'max-plus=', max-plus(1);
    say 'minus-wide=', minus-wide(-$max - 1);
    say 'times-wide=', times-wide($max);
    say 'cmp-wide=', cmp-wide(5), ' ', cmp-wide($max);
}
