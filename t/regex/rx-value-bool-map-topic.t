use v6.c;
use Test;

plan 1;

my $letters := rx/<[a..z]>/;
my $punctuation := rx/<[!/:]>/;

is <a A !>.map({
    if $letters || $punctuation { $_ }
    else { 'X' }
}).join, 'aX!', 'a stored rx value boolifies against the map topic';
