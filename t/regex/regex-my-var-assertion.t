use Test;

# `<$n>` on a lexical the pattern itself declares reads the variable when the
# assertion is matched, not when the pattern is parsed (#13029). Expected
# values verified against rakudo.

plan 12;

ok "x a" ~~ / :my $n; { $n = "x" } <$n> /,
    'a leading :my assigned in a code block';
ok "x a" ~~ / { 1 } :my $n; { $n = "x" } <$n> /,
    'a mid-pattern :my is declared, not X::Undeclared';
ok "x a" ~~ / :my $n = "x"; <$n> /, 'a :my initializer';

is ~("xa" ~~ / :my $n; { $n = "(x)" } <$n> a/), 'xa',
    'a Str value is parsed as a regex, not matched literally';
is ~("xa" ~~ / :my $n = rx/(x)/; <$n> a/), 'xa', 'a Regex value is entered';
is ~("aaab" ~~ / :my $n; { $n = "a+" } <$n> ab /), 'aaab',
    'the interpolated regex backtracks';
is ~("XA" ~~ /:i :my $n; { $n = "x" } <$n> a/), 'XA', ':i reaches the interpolated regex';
is ~("xxa" ~~ / :my $n; { $n = "x" } <$n>+ a/), 'xxa', 'the assertion can be quantified';

{
    my $m = "ab" ~~ / :my $n; { $n = "(a)" } <$n> $<r>=b /;
    is $m<r>.Str, 'b', 'a capture after the assertion is kept';
    is $m.list.elems, 0, "the interpolated regex's captures stay its own";
}

{
    grammar G {
        token TOP { <x("b+")> c }
        token x($p) { a <$p> }
    }
    is G.parse("abbc")<x>.Str, 'abb', "a rule's parameter as <\$p>";
}

throws-like { "ab" ~~ / :my $n; { $n = 'a { 42 }' } <$n> / },
    Exception, message => /'Prohibited regex interpolation'/,
    'a code block in the Str value is refused without MONKEY-SEE-NO-EVAL';
