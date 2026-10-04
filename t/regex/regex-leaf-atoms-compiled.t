use Test;

# The leaf atoms -- builtin calls, backreferences, `<( )>`, `$x`, `<{ … }>`,
# `<~~>` -- are matched by the compiled regex engine through their shared
# definitions (`regex_atom_leaf.rs`), not the walk's single-candidate arm
# (ADR-0135 §8, Slice E, twentieth part). Expected values are rakudo's.

plan 20;

is ~("ab1" ~~ / <alpha>+ /), 'ab', '<alpha> as a call';
is ("ab1" ~~ / <alpha> /)<alpha>.Str, 'a', 'a builtin class call captures under its name';
is ("ab1" ~~ / <x=alpha> /)<x>.Str, 'a', 'an aliased builtin class captures under the alias';
ok ("ab1" ~~ / <x=alpha> /)<alpha>:exists, 'and under its own name';
nok ("ab1" ~~ / <x=.alpha> /)<alpha>:exists, 'but not under its own name with a dot';
is ~("Éa1" ~~ / <:Letter>+ /), 'Éa', 'a Unicode property call';
is ~("a b" ~~ / a <.ws> b /), 'a b', '<.ws> between words';
ok "ab" ~~ / a <!wb> b /, '<!wb> inside a word';
ok "a b" ~~ / a <?wb> /, '<?wb> at a word end';
throws-like { "a" ~~ / <nosuchrule> / }, X::Method::NotFound,
    'an unknown call raises X::Method::NotFound';

is ~("abab" ~~ / (ab) $0 /), 'abab', 'a positional backreference';
is ~("xyxy" ~~ / $<p>=[xy] $<p> /), 'xyxy', 'a named backreference';
is ~("aab" ~~ / (a)+ $0 b /), 'aab', 'a backreference to a quantified capture';
is ~("foobar" ~~ / foo <( bar )> /), 'bar', '<( and )> set the match bounds';

my $v = 'zz';
is ~("azzb" ~~ / a $v b /), 'azzb', 'an outer $x matches its value literally';
is ~("aqqb" ~~ / a :my $w = 'qq'; $w b /), 'aqqb', 'a :my $x matches its value';
is ~("a12" ~~ / a <{ '\d+' }> /), 'a12', '<{ … }> matches the pattern its code yields';
ok "a1" ~~ / a <{ '(\d)' }> /, '<{ … }> with a capture in the yielded pattern matches';
nok ("a1" ~~ / a <{ '(\d)' }> /)[0]:exists, 'and its captures are dropped';

is ~("((()))" ~~ / '(' <~~>? ')' /), '((()))', '<~~> recurses into the regex';
