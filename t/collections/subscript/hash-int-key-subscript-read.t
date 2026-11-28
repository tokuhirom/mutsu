use Test;

# `%h{$int}` on a plain hash reads the entry filed under the Int's decimal Str
# (#19289: the read no longer builds a String for the key). These pin that an
# Int key and its Str spelling address the same entry, and that the shapes the
# fast read must leave alone (object hashes, misses, defaults, bound entries)
# still answer as before.

plan 14;

my %h;
%h{$_} = $_ * 10 for -3..20;

is %h{5}, 50, 'an Int key reads the entry stored under it';
is %h{'5'}, 50, 'the Str spelling reads the same entry';
is %h{-3}, -30, 'a negative Int key';
is %h{0}, 0, 'zero';
my $sum = 0;
$sum += %h{$_ +& 15} for ^32;
is $sum, 2 * ([+] (^16).map(* * 10)), 'Int keys computed in a loop';

my %big;
%big{9223372036854775807} = 'max';
%big{-9223372036854775808} = 'min';
is %big{9223372036854775807}, 'max', 'the largest Int key';
is %big{-9223372036854775808}, 'min', 'the smallest Int key';

nok %h{999}.defined, 'a missing Int key reads an undefined value';
is %h{999}:exists, False, 'and does not autovivify';

my %d is default('none');
%d{1} = 'one';
is %d{1}, 'one', 'a present Int key in a hash with a default';
is %d{2}, 'none', 'a missing Int key reads the default';

my %obj{Any};
%obj{1} = 'int';
%obj{'1'} = 'str';
is %obj{1}, 'int', 'an object hash keeps Int and Str keys apart';
is %obj{'1'}, 'str', 'and reads the Str key separately';

my $x = 'bound';
my %b;
%b{7} := $x;
$x = 'changed';
is %b{7}, 'changed', 'a bound entry read by an Int key follows its variable';
