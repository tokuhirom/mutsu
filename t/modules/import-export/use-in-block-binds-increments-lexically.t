use Test;

# ADR-8751 stage 2, #13836: the user candidates of the built-in increment and
# decrement operators that a block-scoped `use` imports add to the operator
# inside the block only. What no candidate accepts is the native increment's,
# element operands included; a callee written outside the block does not see
# the candidates, and a closure taken in the block keeps them. Checked
# against rakudo.
plan 15;

use lib 't/lib';

sub outside($a) { my $b = $a; $b++; $b.n }
my (&keeps-post, &keeps-pre);
my $c;
{
    use UseInBlockIncrementLexical;
    $c = Ctr.new(n => 1);
    my $x = $c;
    is $x++, 'custom-postinc', 'postfix ++';
    is ++$x, 'custom-preinc', 'prefix ++';
    is $x--, 'custom-postdec', 'postfix --';
    is --$x, 'custom-predec', 'prefix --';
    my $i = 1;
    $i++;
    is $i, 2, 'the built-in postfix ++ still answers';
    ++$i;
    is $i, 3, 'the built-in prefix ++';
    $i--;
    is $i, 2, 'the built-in postfix --';
    my $j = $i++;
    is "$j $i", '2 3', 'a postfix ++ yields the old value';
    my @a = 1, 2;
    @a[0]++;
    is-deeply @a, [2, 2], 'an array element';
    my %h = a => 1;
    %h<a>++;
    is-deeply %h, {a => 2}, 'a hash element';
    is outside($c), 2, 'a callee written outside the block runs the built-in';
    &keeps-post = -> $a { my $y = $a; $y++ };
    &keeps-pre = -> $a { my $y = $a; ++$y };
}
is keeps-post($c), 'custom-postinc', 'a closure keeps postfix ++ after the block';
is keeps-pre($c), 'custom-preinc', 'and prefix ++';
my $k = 5;
$k++;
is $k, 6, 'after the block ++ is the built-in alone';
my $o = $c;
$o++;
is $o.n, 2, 'also for the candidate\'s type';
