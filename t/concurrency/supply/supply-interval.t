use Test;

plan 2;

my $s = Supply.interval(1);
isa-ok $s, Supply, 'Supply.interval returns a Supply';

lives-ok {
    $s.act: -> $v { }
}, 'Supply.interval result can be tapped';
