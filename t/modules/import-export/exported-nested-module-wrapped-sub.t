# A routine a trait wrapped (as `is native` rebinds its `$!do`) is the one a
# call through the exported short package name reaches. Found in FontConfig's
# t/00raw.t: `FcObjectType::get-object-type(...)`, an `our sub ... is native
# {...}` in a class exported from `unit module FontConfig::Raw`, ran its stub
# body ("Stub code executed").
use Test;
use lib 't/lib';
use ExportedNestedWrap;

plan 4;

is Inner::stubbed(1, 2), 'replaced: 1,2', 'short exported name runs the wrapped routine';
is ExportedNestedWrap::Inner::stubbed(3), 'replaced: 3', 'full name runs it too';
is &Inner::stubbed(4), 'replaced: 4', '&-sigiled short name';
is Inner::plain(), 'plain', 'an unwrapped routine is unaffected';
