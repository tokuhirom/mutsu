use Test;

# `\Q...\E` is Perl 5 quotemeta (X::Obsolete) and `//` in term position is an
# empty regex (X::Syntax::Regex::NullRegex), as in rakudo (#18738, roast
# S05-mass/rx.t tests 270, 271, 752).

plan 6;

throws-like '/\Qabc d?\E/', X::Obsolete, '\Q...\E';
throws-like '/\Qabc\E/', X::Obsolete, message => /'\Q as quotemeta'/, 'names quotemeta';
throws-like '//', X::Syntax::Regex::NullRegex, '// is a null regex';
throws-like '"a" ~~ //', X::Syntax::Regex::NullRegex, 'after ~~';
my $x;
is $x // 3, 3, '// as an infix is still defined-or';
is (Nil // 6), 6, 'after a term';
