use Test;

plan 14;

# A `sub <category>:<sym>` name the parser cannot read must say what is wrong
# with it, not backtrack out of the name rule and report the missing block its
# failure left behind.
#
# The assertions read the class and message off the caught exception rather than
# using `throws-like`'s type argument: mutsu's native `throws-like` does not
# check it, so a type-only assertion here passes even against the generic parse
# failure this fix replaces.

sub compile-error($code) {
    my $caught;
    { EVAL $code; CATCH { default { $caught = $_ } } }
    $caught // Nil;
}

# A colon-pair value that cannot spell a name is TooComplex. Note this check
# comes BEFORE the category check: `meow:[bar]` is TooComplex, not Category,
# even though `meow` is not a category either.
my $regex-val = compile-error 'sub infix:[/./] { 42 }';
ok $regex-val ~~ X::Syntax::Extension::TooComplex,
    'a regex as a colon pair value is TooComplex';
like $regex-val.message, /"too complex to use in name"/, 'TooComplex names the problem';
like $regex-val.message, /'/./'/, 'and quotes the offending value';

my $bare-val = compile-error 'sub infix:[bar] { 42 }';
ok $bare-val ~~ X::Syntax::Extension::TooComplex,
    'an undeclared bare word as a colon pair value is TooComplex';

my $meow-bracket = compile-error 'sub meow:[bar] { 42 }';
ok $meow-bracket ~~ X::Syntax::Extension::TooComplex,
    'the colon pair value is checked before the category';

# An unknown operator category is Category.
my $angle = compile-error 'sub meow:<bar> { }';
ok $angle ~~ X::Syntax::Extension::Category,
    'an unknown category with an angle-bracket symbol is Category';
like $angle.message, /"Cannot add tokens of category 'meow'"/,
    'Category names the offending category';

my $guillemet = compile-error 'sub meow:«bar» { }';
ok $guillemet ~~ X::Syntax::Extension::Category,
    'an unknown category with a guillemet symbol is Category';

# Legal spellings must keep parsing, and must not raise either class.
nok compile-error('sub meow:foo<bar> { 42 }').defined,
    'an extended sub name is not an operator declaration';
nok compile-error('sub infix:["@"] ($a, $b) { $a + $b }').defined,
    'a quoted bracket operator name still compiles';
{
    sub meow:foo<bar> { 42 }
    is meow:foo<bar>(), 42, 'and an extended sub name is callable';
}
{
    sub infix:["@"] ($a, $b) { $a + $b }
    is (1 @ 2), 3, 'a quoted bracket operator name still declares an operator';
}
{
    constant opsym = "@";
    sub infix:[opsym] ($a, $b) { $a * $b }
    is (3 @ 4), 12, 'a constant as a bracket operator name still works';
}
{
    sub infix:<%%%> ($a, $b) { $a - $b }
    is (9 %%% 4), 5, 'an ordinary angle-bracket operator name still works';
}
