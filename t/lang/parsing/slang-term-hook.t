use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's grammar role may add new `term:sym<...>` and `number:sym<...>`
# productions (ADR-15762 slice 2, #13370): the slang's token and action run where
# the parser starts a term, and the RakuAST node the action attaches is the value.

plan 9;

use TermSlang;

is answer, 42, 'a slang term is a value';
is answer + 1, 43, 'it takes part in an expression';
is roman:XIV, 14, 'a slang number production reads its digits';
is roman:MCMXC, 1990, 'and runs its action over them';
is-deeply (roman:IX, answer), (9, 42), 'productions are terms of a list';

sub answerish { 7 }
is answerish(), 7, 'an identifier that starts with the term is not the term';
my $answer-ish = 3;
is $answer-ish, 3, 'neither is a variable';

my @squares = (1, 2, 3).map({ $_ * answer });
is @squares.join(','), '42,84,126', 'the term works inside a block';

throws-like 'answer', Exception, 'an EVAL string does not see the slang';
