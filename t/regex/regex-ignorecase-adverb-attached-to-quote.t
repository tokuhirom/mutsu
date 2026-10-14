use Test;

# From CSS::Module's `rule media-op {:i'only'|'not'}`: a `:i` written directly
# against a quote scopes over the whole alternation, like `:i 'only'|'not'`.

plan 7;

grammar G {
    rule spaced   { :i 'only' | 'not' }
    rule attached { :i'only' | 'not' }
    rule grouped  { :i['only' | 'not'] }
}

for <spaced attached grouped> -> $rule {
    ok G.subparse('NOT', :rule($rule)).so, "$rule: second branch is case-insensitive";
}
ok G.subparse('ONLY', :rule<attached>).so, 'first branch too';
ok "NOT" ~~ /:i'only'|'not'/, 'in a plain regex';
nok "NOT" ~~ /'only'|'not'/, 'without :i it is still case-sensitive';
ok "ONLY" ~~ /:i'only'|'not'/, 'first alternative in a plain regex';
