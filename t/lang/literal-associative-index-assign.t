use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 7;

throws-like { 5<a> = 3 }, X::AdHoc, message => 'Type Int does not support associative indexing.',
    'Int literal';
throws-like { "s"<a> = 3 }, X::AdHoc, message => 'Type Str does not support associative indexing.',
    'Str literal';
throws-like { 1.5<a> = 3 }, X::AdHoc, message => 'Type Rat does not support associative indexing.',
    'Rat literal';
throws-like { True<a> = 3 }, X::AdHoc, message => 'Type Bool does not support associative indexing.',
    'Bool literal';
throws-like { (5)<a> = 3 }, X::AdHoc, message => 'Type Int does not support associative indexing.',
    'parenthesized literal';

is_run '5<a> = say "rhs"', { :out("rhs\n"), :err(/'Type Int does not support associative indexing.'/) },
    'the right-hand side runs before the indexing error';

my %h;
%h<a> = 3;
is %h<a>, 3, 'a hash store still assigns';
