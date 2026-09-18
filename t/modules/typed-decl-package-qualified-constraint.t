use Test;

# A typed declaration whose constraint is an unqualified USER type must keep
# the package-qualified identity the type checker uses, so container metadata
# renders `Array[Outer::Inner]` rather than `Array[Inner]`.
#
# The declaration op resolves that against the package chain on EVERY
# execution, so a core constraint (`int`, `Str`, ...) is skipped: nothing can
# shadow it, and the walk it would do ends in a linear scan of the whole type
# registry. This file pins both halves — the user type still resolves, and the
# core ones still declare exactly what they spell.

plan 8;

module Outer {
    class Inner { }

    our sub typed-metadata() {
        my Inner @rows;
        my Inner $one;
        (@rows.WHAT.^name, $one.WHAT.^name)
    }

    our sub core-metadata() {
        my int $i = 3;
        my Str $s = 'x';
        my Int @ints;
        my Str %strs;
        ($i, $s, @ints.WHAT.^name, %strs.WHAT.^name)
    }
}

my ($array-name, $scalar-name) = Outer::typed-metadata();
is $array-name, 'Array[Outer::Inner]',
    'an unqualified user type in a typed array declaration stays package-qualified';
is $scalar-name, 'Outer::Inner',
    'an unqualified user type in a typed scalar declaration stays package-qualified';

my ($i, $s, $ints-name, $strs-name) = Outer::core-metadata();
is $i, 3, 'a native int declaration inside a module still binds its value';
is $s, 'x', 'a core Str declaration inside a module still binds its value';
is $ints-name, 'Array[Int]',
    'a core type constraint is not rewritten to a package-qualified name';
is $strs-name, 'Hash[Str]',
    'a core hash value constraint is not rewritten to a package-qualified name';

# A user declaration that really does shadow a core name must still win, which
# is why the skip is conditional rather than a flat "core names never resolve".
module Shadowed {
    subset Str of Int where * > 0;

    our sub shadowed-metadata() {
        my Str @positives;
        @positives.WHAT.^name
    }
}

is Shadowed::shadowed-metadata(), 'Array[Shadowed::Str]',
    'a package-local subset shadowing a core name still resolves qualified';

# The same shape at file scope has no enclosing package to qualify against, so
# the spelling survives untouched.
my Int @plain;
is @plain.WHAT.^name, 'Array[Int]', 'a file-scope core constraint is unchanged';
