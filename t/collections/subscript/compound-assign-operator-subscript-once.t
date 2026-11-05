use Test;
plan 7;
my %h; my $i = 7;
%h{$i % 5} += 1; %h{$i % 5} += 1;
is %h<2>, 2, 'operator subscript accumulates';
my $calls = 0;
sub k { $calls++; 'k' }
%h{k()} += 3;
is $calls, 1, 'a call subscript is evaluated once';
my @a = 0, 0, 0; my $j = 0;
@a[$j++] += 5;
is-deeply @a, [5, 0, 0], 'a ++ subscript is evaluated once';
is $j, 1, 'and advanced once';
my $n = 10;
for ^3 { %h{$n % 3 + $_} *= 2 if False; %h{$n % 4 ~ 'x'} ~= 'a' }
is %h<2x>, 'aaa', 'string-concat compound with operator subscript';
my %g;
%g{$i + 1} //= 9; %g{$i + 1} //= 1;
is %g{8}, 9, '//= with operator subscript';
my @b = 1, 2, 3;
@b[$i % 3] *= 10;
is-deeply @b, [1, 20, 3], 'positional operator subscript';
