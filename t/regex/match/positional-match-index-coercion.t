use Test;

plan 6;

my $match = '1' ~~ /\d/;
is <a b c>[$match], 'b', 'a Match numifies for a positional List index';
is <a b c>.Seq[$match], 'b', 'a Match numifies for a positional Seq index';
my @items = <a b c>;
is @items[$match], 'b', 'a Match numifies for a positional Array index';
is @items[$match, 2].join(','), 'b,c', 'a Match numifies inside a positional slice';

class Index {
    method Int() { 2 }
}
is @items[Index.new], 'c', 'user-defined Int coercion still works';
@items[$match] = 'x';
is @items.join(','), 'a,x,c', 'a Match index also selects an Array assignment slot';
