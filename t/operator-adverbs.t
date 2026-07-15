use v6;
use Test;

plan 10;

# A trailing colonpair adverb binds to the expression's outermost operator
# (advent2013-day10): user infix on builtin tokens, prefix ops, precedence.

my $applies = 'zilch';

sub infix:</>($a, $b, :$round) { $applies = '/:round' if $round; }
1 / 3 :round;
is $applies, '/:round', ':round applies to user infix </>';

sub infix:<+>($a, $b, :$adv) { $applies = "+{$b}" if $adv; }
sub infix:<->($a, $b, :$adv) { $applies = "-{$b}" if $adv; }
$applies = 'zilch';
1 + 2 - 3 :adv;
is $applies, '-3', 'left-assoc chain: adverb applies to the outermost (last) infix';

sub infix:<**>($a, $b, :$adv) { $applies = "{$a}**" if $adv; 0 }
$applies = 'zilch';
1 ** 2 ** 3 :adv;
is $applies, '1**', 'right-assoc chain: adverb applies to the outermost (leftmost) **';

# user overrides of builtin tokens dispatch even without adverbs
is (5 - 2), Nil, 'user infix:<-> overrides native Int-Int';
sub infix:<*>($a, $b) { 'u*' }
is (2 * 3), 'u*', 'user infix:<*> overrides native Int-Int';
is (2 ** 3), 0, 'user infix:<**> overrides native pow';

# prefix op adverbs: outermost prefix gets the adverb, not the inner call
class Foo { method bar(:$adv) { $applies = '.bar()' if $adv; } }
my $foo = Foo.new;
sub prefix:<!>($a, :$adv) { $applies = '!' if $adv; }
$applies = 'zilch';
!$foo.bar() :adv;
is $applies, '!', 'adverb applies to prefix, not the inner method call';

$applies = 'zilch';
!($foo.bar() :adv);
is $applies, '.bar()', 'parens redirect the adverb to the method call';

# an Empty operand does not vanish from an operator call
lives-ok { !$foo.bar() }, 'Empty operand to a user prefix op still dispatches';

# :nth[...] square-bracket form is a grouped compile error
throws-like { EVAL 'my $d = "f fo foo"; $d ~~ m:nth[5]/fo+/' },
    X::Comp::Group, 'm:nth[5] square parens throw X::Comp::Group';
