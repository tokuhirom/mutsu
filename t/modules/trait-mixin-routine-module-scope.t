use Test;
use lib 't/lib-trait-mixin-routine';

# A trait that composes a role into a module's routine (`$r does R`, as
# NativeCall's `is native` does) must not leave the routine bound as `&name`
# in the importer: `TraitMixinOuter` only `need`s `TraitMixinOuter::Raw`, so
# its own exported `compress` is the one the importer's `&compress` names
# (PDF's Flate filter called Compress::Zlib::Raw's native `compress` through
# `.&compress`, #12907). The composed CALL-ME role is honoured by every call
# form of the routine itself. Expected values are the `raku` output.

plan 9;

use TraitMixinOuter;

is compress(1), 'outer:called-me', 'a by-name call reaches the imported routine';
is 1.&compress, 'outer:called-me', '.&name reaches the imported routine';
is &compress(1), 'outer:called-me', '&name() reaches the imported routine';
is TraitMixinOuter::Raw::compress(1), 'called-me', 'a qualified call goes through CALL-ME';
is &TraitMixinOuter::Raw::compress(1), 'called-me', 'a qualified &name() goes through CALL-ME';
my &c = &TraitMixinOuter::Raw::compress;
is c(1), 'called-me', 'a code value of the routine goes through CALL-ME';
is &TraitMixinOuter::Raw::compress.^name, 'Sub+{TraitMixinOuter::Raw::Marked}', 'the code value carries the role';
is TraitMixinOuter::Raw::probe(), 'called-me/called-me', "the module's own calls go through CALL-ME after its load";
is EVAL(q[use TraitMixinOuter::Raw; compress(1) ~ "/" ~ &compress(1)]), 'called-me/called-me',
    'an importer of the routine itself gets the composed routine';
