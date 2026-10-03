use v6;
use lib 't/lib';
use Test;

# A `sub EXPORT` map entry `'&term:<answer>' => &answer` makes the bareword
# `answer` a call to the routine in the importer, as `sub term:<answer> is
# export` does.

use ExportSubTermSymbol;

plan 3;

is answer, 42, 'the exported term is called as a bareword';
is answer + 1, 43, 'it composes as a term in an expression';
is answer-calls, 2, 'each bareword use is one call';
