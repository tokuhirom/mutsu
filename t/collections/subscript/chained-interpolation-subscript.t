use Test;

plan 1;

my @units = %(plural => 'seconds');
is "0 @units[*-1]<plural>", '0 seconds', 'interpolated postcircumfixes chain after an array index';
