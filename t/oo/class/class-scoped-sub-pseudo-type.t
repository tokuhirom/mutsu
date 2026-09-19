# `::?CLASS` in an ordinary sub declared by a class is the class's type, not
# an unconstrained pseudo-type. Math::Quaternion's exported operator multis
# exposed this when their candidates incorrectly matched Arrays.
use Test;

unit class ClassScopedPseudoType;

multi sub infix:<class-scoped-eqv>(::?CLASS:D $a, ::?CLASS:D $b) is export {
    $a.^name ~ $b.^name
}

multi sub infix:<class-scoped-eqv>(Any:D $a, Any:D $b) is export {
    'fallback'
}

plan 3;
is ClassScopedPseudoType.new class-scoped-eqv ClassScopedPseudoType.new,
    'ClassScopedPseudoTypeClassScopedPseudoType',
    'class-scoped pseudo-type candidate accepts the declaring class';
is [1, 2] class-scoped-eqv [3, 4], 'fallback',
    'class-scoped pseudo-type candidate rejects Arrays';
is ([1, 2] eqv [1, 2]), True,
    'class-scoped operators do not intercept unrelated Array eqv';
