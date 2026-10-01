# #10393: a bare `our $x;` in a module file holds the `Any` type object,
# not `Nil`, from the module's routines, `$Mod::x` and the importer.
use v6;
use lib 't/lib';
use Test;
use OurBareDefault;

plan 4;

ok get-x() === Any, 'module routine sees Any';
ok OurBareDefault::get-qualified() === Any, 'our sub via $Mod::x sees Any';
ok $OurBareDefault::x === Any, 'importer reads $Mod::x as Any';
is $OurBareDefault::x.raku, 'Any', '.raku is Any';
