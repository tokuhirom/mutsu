use Test;

plan 6;

sub root(:$deps is raw) { $deps = 5 }
sub w3(*%h) { root |%h }
my $k;
w3 :deps($k);
is $k, 5, '|%h forwards the caller container of a named is raw argument';

# the slurpy hash itself still reads plain values
sub peek(*%h) { %h<a> ~ "/" ~ %h.values.join(",") ~ "/" ~ %h.raku }
my $v = 3;
is peek(:a($v)), "3/3/\{:a(3)\}", 'values in *%h read as plain values';
is $v, 3, 'the caller variable is untouched when nothing writes it';

# the hash is a copy as far as assignment to its entry goes
sub bump(*%h) { %h<a> = 9; %h<a> }
my $w = 1;
is bump(:a($w)), 9, 'assigning an entry of *%h works';

# a non-variable named argument is forwarded as a value
sub w4(*%h) { root |%h }
dies-ok { w4 :deps(7) }, 'a literal named argument is not writable';

# forwarded together with a positional
sub root2($x, :$out is raw) { $out = $x * 2 }
sub w5($x, *%h) { root2 $x, |%h }
my $o;
w5 4, :out($o);
is $o, 8, 'positional plus |%h';
