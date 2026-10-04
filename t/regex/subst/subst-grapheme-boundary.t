use Test;
# From Lang::JA::Kana: a literal-string `.subst` pattern only matches on
# grapheme boundaries (half-width kana + U+FF9E is one grapheme).
plan 6;

my $ka = "\x[FF76]\x[FF9E]";
is $ka.subst("\x[FF76]", "Y"), $ka, 'no match inside a grapheme';
is $ka.subst("\x[FF76]", "Y", :g), $ka, ':g does not split a grapheme either';
is "$ka\x[FF77]".subst("\x[FF76]", "Y", :g), "$ka\x[FF77]", 'unrelated tail untouched';
is "$ka\x[FF76]".subst("\x[FF76]", "Y", :g), "{$ka}Y", 'matches later whole grapheme';
is "$ka\x[FF77]\x[FF9E]".subst($ka, "X", :g), "X\x[FF77]\x[FF9E]", 'whole-grapheme pattern matches';
is "aXbXc".subst("X", "-", :g), "a-b-c", 'plain literal still works';
