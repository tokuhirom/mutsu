use Test;

# A user `multi postfix:<++>` makes every `++` a call of the operator; an
# element operand its candidates do not take is still incremented in place by
# the native increment (#13836). Checked against rakudo.
plan 4;

class Ctr { has $.n }
multi sub postfix:<++>(Ctr $a is rw) { 'custom' }
multi sub prefix:<-->(Ctr $a is rw) { 'custom' }

my @a = 1, 2;
@a[0]++;
is-deeply @a, [2, 2], 'an array element';
my %h = a => 1;
%h<a>++;
is-deeply %h, {a => 2}, 'a hash element';
--@a[1];
is-deeply @a, [2, 1], 'a prefix on an element';
my $c = Ctr.new(n => 1);
is $c++, 'custom', 'the candidate still answers its type';
