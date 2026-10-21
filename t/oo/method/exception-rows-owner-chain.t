use Test;

plan 8;

my $adhoc = X::AdHoc.new(:payload('le supply quit'));
is $adhoc.message, 'le supply quit', 'X::AdHoc.message is its payload';
is $adhoc.payload, 'le supply quit', 'X::AdHoc.payload';
is $adhoc.Str, 'le supply quit', 'X::AdHoc.Str';
is $adhoc.gist, 'le supply quit', 'X::AdHoc.gist';

class X::Mine is Exception { method message { 'computed' } }
is X::Mine.new.message, 'computed', 'a user exception keeps its own message';
is X::Mine.new.gist, 'computed', 'and its gist renders it';

is X::OutOfRange.new(:what<x>, :got(5), :range<1..3>).message,
    'x out of range. Is: 5, should be in 1..3', 'a built-in X:: class formats its message';
is Exception.new.backtrace.^name, 'Nil', 'an unthrown Exception has no backtrace';
