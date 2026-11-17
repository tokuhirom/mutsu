use Test;

# The separator of `%` / `%%` may be a variable: a lexical read at match time
# (`% $sep`), or a dynamic variable whose value the interpolation pre-pass
# splices in (`% $*S`). Either is one atom, however long its value
# (#17891). Expectations were measured against rakudo 2026.09.

plan 7;

{
    my $sep = ",";
    is ~("a,b,c" ~~ /[\w+]+ % $sep/), 'a,b,c', 'a lexical separator';
    ok "a,b,c" ~~ /^ [\w+]+ % $sep $/, 'followed by the end anchor';
}
{
    my $*S = ";";
    is ~("a;b;c" ~~ /[\w]+ % $*S/), 'a;b;c', 'a dynamic separator';
    is ~("a;b;c" ~~ /\w+ % $*S /), 'a;b;c', 'with whitespace after it';
    is ~("a;b;" ~~ /\w+ %% $*S/), 'a;b;', 'with %%';
}
{
    my $*T = "--";
    is ~("a--b--c" ~~ /\w+ % $*T/), 'a--b--c', 'a multi-character dynamic separator';
}
{
    my $long = "::";
    is ~("x::y::z" ~~ /\w+ % $long/), 'x::y::z', 'a multi-character lexical separator';
}
