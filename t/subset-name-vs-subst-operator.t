use Test;

# A subset name used as a term (e.g. `$x ~~ S`) must parse as the type, not as
# the `S///` non-destructive-substitution operator. `~~ S/.../.../ ` is valid, so
# a bareword `S` (or any subset name) after `~~` was otherwise swallowed as a
# substitution — especially when a trailing `, 'desc'` and a following line
# provided the `S,pat,repl,` delimiters. S12-subset/subtypes.t test 68.

plan 6;

{
    role R { };
    subset S of R;
    nok 1 ~~ S,  'a role-based subset name after ~~ is a type, not S///';
    ok  R ~~ S,  'the role type object matches its subset';
}

# A real S/// still parses as a substitution when S is not a declared type.
{
    my $x = 'abc';
    my $y = S/b/X/ given $x;
    is $x, 'abc', 'S/// is non-destructive (source unchanged)';
    is $y, 'aXc', 'S/// returns the substituted copy';
}

# `my subset` name is also a term after ~~.
{
    my subset Big of Int where * > 100;
    ok  (200 ~~ Big), 'my-subset name after ~~ matches';
    nok (3   ~~ Big), 'my-subset name after ~~ rejects';
}
