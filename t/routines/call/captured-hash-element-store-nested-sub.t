use Test;

# Element stores into a variable a nested `my sub` captures (the
# MCP::Server::Tool::Web tokenizer's `%memo{$needle} = [...]`) resolve the
# target by name: they must reach the enclosing routine's container, through
# a sigilless alias too.

plan 9;

sub tokenize(@needles) {
    my %memo;
    my @order;
    my sub remember(Str:D $needle, Int:D $at) {
        %memo{$needle} = [$at, $at + 1];
        @order[@order.elems] = $needle;
        %memo{$needle}[1]
    }
    my $last;
    $last = remember($_, $++) for @needles;
    (%memo, @order, $last)
}

my ($memo, $order, $last) = tokenize(<a b a c>);
is-deeply $memo<a>, [2, 3], 'a re-stored key holds the last array';
is-deeply $memo<b>, [1, 2], 'another key';
is $memo.elems, 3, 'one entry per distinct key';
is-deeply $order.List, <a b a c>, 'a captured array grows by index store';
is $last, 4, 'the store answers through the element read';

sub through-alias() {
    my %h;
    my \alias = %h;
    my sub put($k, $v) { alias{$k} = $v }
    put('x', 1);
    put('y', [2, 3]);
    %h
}
my %a = through-alias();
is %a<x>, 1, 'a sigilless alias of a captured hash stores into it';
is-deeply %a<y>, [2, 3], 'an array through the alias';

our %pkg;
sub fill() { my sub inner($k) { %pkg{$k} = $k.uc }; inner($_) for <p q> }
fill();
is %pkg<p>, 'P', 'an `our` hash from a nested sub';
is %pkg.elems, 2, 'both stores land';
