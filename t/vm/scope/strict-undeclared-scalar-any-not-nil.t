use Test;

plan 8;

# A never-declared scalar reads as the `Any` type object, not `Nil` —
# whether it is read bare under `no strict` (raku auto-declares it as a
# package variable defaulting to Any) or as an explicitly package-qualified
# name, which auto-vivifies the same way regardless of `strict`/`no strict`.
# Before this fix, `GetGlobal`'s not-found fallback always returned `Value::
# NIL`, so `Nil` (which swallows method calls) leaked into places raku would
# have an autovivified, still-callable `Any`.
# https://github.com/tokuhirom/mutsu/issues/9775

{
    no strict;
    is $no_strict_undeclared_scalar.WHAT, (Any), 'bare undeclared scalar under no strict is (Any)';
    ok $no_strict_undeclared_scalar.WHAT === Any, 'bare undeclared scalar under no strict === Any, not Nil';
}

is $NoStrictUndeclaredScalarPkg::qualified.WHAT, (Any),
    'undeclared package-qualified scalar is (Any)';
ok $NoStrictUndeclaredScalarPkg::qualified.WHAT === Any,
    'undeclared package-qualified scalar === Any, not Nil (strict has no say over `::` names)';

{
    no strict;
    is [:$no_strict_undeclared_pair_val].raku, '[:no_strict_undeclared_pair_val(Any)]',
        ':$name pair shorthand over an undeclared scalar renders Any, not Nil';
}

# The pre-existing magic-var defaults must stay Nil — only a genuinely
# undeclared ordinary/package-qualified scalar changes.
is $/.raku, 'Nil', '$/ default is still Nil';
is $!.raku, 'Nil', '$! default is still Nil';
is $0.raku, 'Nil', '$0 (unmatched capture) default is still Nil';
