use Test;

# annotations 0.1.0: `my package EXPORT { OUR::<DEFAULT> := Mod }` makes the
# symbol table of `Mod` the module's default export list.

plan 3;

use lib 't/lib';
use ExportDefaultPackageMod;

is exported-sub(), "sub", 'an `our sub` of the bound package is imported';
is ExportedClass.new.hi, "hi", 'a class bound with OUR::<Name> is imported';
is Aliased.new.who, "hidden", 'a lexical class re-bound under another key is imported';
