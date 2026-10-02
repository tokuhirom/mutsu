use v6;
use Test;

# A `<` inside a quoted regex literal is a literal character, so a variable
# after it still interpolates. The scalar-interpolation pass skipped the `<`
# as the start of an angle construct and swallowed the pattern up to the next
# `>`, so `/ '<' $tag /` never matched (Template::HAML's `find-and-preserve`,
# #10638).

plan 7;

my $tag = 'pre';
ok '<pre>' ~~ / '<' $tag /, "single-quoted '<' then a variable";
ok '<pre>' ~~ / "<" $tag /, 'double-quoted "<" then a variable';
ok '<xpre>' ~~ / '<' x $tag '>' /, 'literal atoms between';
nok '<pra>' ~~ / '<' $tag /, 'the variable is still matched as its value';

is "<pre>a\nb</pre>".subst(
    / '<' $tag (<-[>]>*) '>' (.*?) '</' $tag '>' /,
    -> $m { '<' ~ $tag ~ $m[0] ~ '>' ~ $m[1].subst("\n", '&#x000A;', :g) ~ '</' ~ $tag ~ '>' },
    :g),
    '<pre>a&#x000A;b</pre>', 'the find-and-preserve substitution';

ok '<pre' ~~ / '<' <alpha>+ /, 'an angle construct after a quoted < still parses';
ok 'a<b' ~~ / a '<' b /, 'a quoted < with no variable';
