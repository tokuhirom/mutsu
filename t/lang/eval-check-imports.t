use lib 't/lib';
use Test;

# An EVAL'd unit is checked for undeclared names, routines and variables
# before it runs (rakudo's CHECK time). These are names the unit does declare,
# or imports, that the check used to miss (#13295); each matches rakudo 2026.09.

plan 7;

is EVAL(q[constant term:<$bar> = 42; $bar + 1]), 43,
    'a sigiled term constant declares its term';
is EVAL(q[grammar G13295 { token value:sym<number> { \d+ }; token TOP { <value:sym<number>> } }; G13295.parse("12").Str]),
    '12', 'a `:sym<...>` subrule adverb is no term';
is EVAL(q[use EnumTaggedExportFixture :PlainTag; import EnumTaggedExportFixture :BareTag; C]).key, 'C',
    'an enum value a module exports under a tag';
is EVAL(q[use RakuASTModuleNames::Greeter; Greeter.^name]), 'RakuASTModuleNames::Greeter',
    'a qualified role exported under its short name';
is EVAL(q[use ExportHookOtherTerm :u; other]), 42,
    'a term a `sub EXPORT` hook installs';
is EVAL(q[module N13295 { our $v is export(:t) = 3 }; import N13295 :t; $v]), 3,
    'a variable an `import` of a unit-declared module brings in';
is EVAL(q[my $r = ((my $a, my $b) »=» (1, 2)); "$a $b"]), '1 2',
    'declarations inside a hyper-assignment target';
