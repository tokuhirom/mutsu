# A `&name` loop parameter is a lexical `&name` for the body: a bare `name(...)`
# call reaches it, not a same-named routine. Found via Test::Describe, whose
# `for @!its -> &it { it |%pars }` shadows its exported `sub it`.
use Test;

plan 3;

sub it(|) { 'GLOBAL it' }

my @blocks = -> *%h { "first {%h<x> // ''}" }, -> *%h { "second {%h<x> // ''}" };
is-deeply (gather for @blocks -> &it { take it() }), ('first ', 'second '),
    'a bare call reaches the loop parameter';
my %p = x => 1;
is-deeply (gather for @blocks -> &it { take it |%p }), ('first 1', 'second 1'),
    'also with a slipped argument list';
is it(), 'GLOBAL it', 'outside the loop the routine is called';
