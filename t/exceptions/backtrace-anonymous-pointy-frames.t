use Test;

plan 3;

sub zipi { { { die 'bad' }() }() }
try zipi;
my $bt = $!.backtrace;

ok $bt.list.grep({ .subname eq '' }).elems >= 2,
    'nested anonymous blocks keep empty subnames';
my $named = $bt.next-interesting-index(:named);
is $bt[$named].subname, 'zipi',
    ':named skips anonymous blocks and selects their enclosing routine';
ok $bt.nice(:oneline).contains('in sub zipi'),
    'one-line rendering names the enclosing routine';
