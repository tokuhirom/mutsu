use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A `&sub` of the unit being parsed, passed to a slang's `use`, reaches its
# `sub EXPORT` and is callable at parse time (#17055).

plan 3;

sub decimal { 'unused' }

sub integer { "i$^a" }

is 5, '5', 'a literal before the slang is untouched';

use NumishHandlerSlang &integer, &decimal;

is 7, 'i7', 'the handler runs on each Int literal after the use';
is (1, 2).join(','), 'i1,i2', 'in list elements too';
