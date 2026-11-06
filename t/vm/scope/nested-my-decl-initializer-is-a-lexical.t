use v6;
use Test;

# `my $f = my $g = EXPR` declares `$g` in the enclosing scope, like a statement
# of its own: a closure or named sub that follows sees and writes the very same
# variable. The inner declaration used to get no local slot, so those writes
# were lost. Found with Test::Describe (`my &ROOT = my $DESCRIBE = ...`).
# Identical under raku.

plan 6;

my $f = my $g = 2;
is $f, 2, 'outer initialized from the inner declaration';
my $h = sub { $g = 9 };
$h();
is $g, 9, 'a closure writes the inner variable';
sub sg() { $g = 11 }
sg();
is $g, 11, 'a named sub writes the inner variable';
is $f, 2, 'the outer variable is a copy';

my $k = (my $m = 3);
sub sm() { $m = 13 }
sm();
is $m, 13, 'parenthesized inner declaration, named sub';

my &r = my $d = -> { 1 };
sub setd($v) { $d = $v }
setd(5);
is $d, 5, 'a & outer with a scalar inner declaration';
