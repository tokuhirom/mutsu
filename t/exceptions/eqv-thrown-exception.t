use Test;

# From Test::Stream: a thrown exception and a freshly constructed one of the
# same class render alike with `.raku`, so they are eqv (hidden state such as
# the backtrace is not compared).

plan 3;

my $thrown = try { die 'x' };
my $e = $!;
my $c = X::AdHoc.new(payload => 'x');
ok $e eqv $c, 'a thrown X::AdHoc is eqv to a constructed one';
ok ${:exception($e)} eqv ${:exception($c)}, 'also inside a hash';
nok $e eqv X::AdHoc.new(payload => 'y'), 'a different payload is not eqv';
