use Test;

# A subset name used as a term (e.g. `$x ~~ S`) must parse as the type, not as
# the `S///` non-destructive-substitution operator. `~~ S/.../.../ ` is valid, so
# a bareword `S` (or any subset name) after `~~` was otherwise swallowed as a
# substitution — especially when a trailing `, 'desc'` and a following line
# provided the `S,pat,repl,` delimiters. S12-subset/subtypes.t test 68.

plan 6;

# A real S/// parses as a substitution as long as `S` is not declared. This
# block must come FIRST: a non-`my` `subset S` is installed package-scoped, so
# from its declaration onwards `S` is a term everywhere and the `S///` quote
# language spelled `S` is gone — rakudo parses `S/b/X/` after such a declaration
# as the division `S / b / X /` and reports `b` as an undeclared routine.
{
    my $x = 'abc';
    my $y = S/b/X/ given $x;
    is $x, 'abc', 'S/// is non-destructive (source unchanged)';
    is $y, 'aXc', 'S/// returns the substituted copy';
}

{
    role R { };
    subset S of R;
    nok 1 ~~ S,  'a role-based subset name after ~~ is a type, not S///';
    ok  R ~~ S,  'the role type object matches its subset';
}

# `my subset` name is also a term after ~~.
{
    my subset Big of Int where * > 100;
    ok  (200 ~~ Big), 'my-subset name after ~~ matches';
    nok (3   ~~ Big), 'my-subset name after ~~ rejects';
}
