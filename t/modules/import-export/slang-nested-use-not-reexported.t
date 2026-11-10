use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang module that merely `use`s another slang does not hand that slang on
# to its own importer: `$*LANG` is the language of the unit being compiled, so
# the nested module's `define_slang` slanged the outer module's compilation
# only (#17267, Qwiratry::Mold::Slang and the Topic slang).

plan 3;

use NestedSlangOuter;

my @*TRACE;
my $r;
given 3 { $r = $_ * 2 }
is $r, 6, 'a plain given still runs its block with the topic';
is-deeply @*TRACE, [], 'the nested slang did not rewrite the importer\'s given';
is (given 4 { $_ + 1 }), 5, 'a given expression is unchanged too';
