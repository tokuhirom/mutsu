use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's action can ask `self.^name.starts-with('Raku::')` to tell the RakuAST
# frontend from the legacy one (Slang::Roman, Slang::Date). The classes the
# slang host composes are named `Raku::Grammar` / `Raku::Actions` for that.

plan 2;

use RakuNameSlang;

is 0t7, 21, 'the action saw a Raku:: class and built the RakuAST literal';
is 0t10 + 1, 31, 'in an expression too';
