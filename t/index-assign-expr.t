use Test;
plan 2;

my %h = a => 1;
%h<a> = 5;
is %h<a>, 5, 'index assignment statement updates hash element';

my %nested;
%nested<a><b> = 9;
is %nested<a><b>, 9, 'chained index assignment still works';
