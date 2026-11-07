use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's `sigilless-variable` token lets `my NAME` declare a variable with no
# sigil (Slang::Nogil: an identifier, Slang::Emoji: one Symbol-other character).

plan 10;

use SigillessVariableSlang;

my a = 42;
is a, 42, 'an identifier names a variable';
is a + a, 84, 'it works in expressions';

my @a = ^100;
is @a[a], 42, 'and as an index';

my %h = a => 666;
is %h<a>, 666, 'but an identifier before => is still a quoted key';

my 👍 = 42;
is 👍, 42, 'a symbol character names a variable';
is 👍 + 👍, 84, 'it works in expressions';

my @b = ^100;
is @b[👍], 42, 'and as an index';

my %i = 👍 => 666;
is %i{👍}, 666, 'a symbol before => is a term, not a quoted key';

my Int n = 7;
is n, 7, 'a declaration can carry a type';
is-deeply (n, 👍), (7, 42), 'both in one list';
