use v6;
use Test;
use lib 't/lib';
use PcRoundedLib;

# The same as user-postcircumfix-index-assign.t with the candidates imported
# from a module (Array::Rounded's shape): a block's `use` binds them, and the
# element store has to ask them too (#16294).
plan 5;

my @a is Rounded = <a b c d e>;
is @a[1.5], 'c', 'a read goes through the imported candidate';
@a[1.5] = 'X';
is @a.join(','), 'a,b,X,d,e', 'an assignment goes to the rounded index';
@a[3.4] := 'Y';
is @a.join(','), 'a,b,X,Y,e', 'a binding goes to the rounded index';
@a[0] = 'Z';
is @a.join(','), 'Z,b,X,Y,e', 'an integer index is unchanged';

my @plain = <a b c d e>;
@plain[1.5] = 'P';
is @plain.join(','), 'a,P,c,d,e', 'a plain array keeps the native index';
