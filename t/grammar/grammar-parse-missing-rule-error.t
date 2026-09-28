use Test;

plan 3;

# A `.parse`/`.parsefile` whose start rule has no `token`/`rule`/`regex`/proto
# definition (nor a plain `method` of that name) used to leak the internal
# dispatcher's own message ("Unknown method value dispatch (fallback
# disabled): parse") instead of naming the actually-missing rule -- raku
# reports it as a plain missing method on the grammar (#9795).

grammar TOPlessGrammar {
    token x { a }
}

throws-like { TOPlessGrammar.parse("a") }, X::Method::NotFound,
    message => /'No such method \'TOP\' for invocant of type \'TOPlessGrammar\''/,
    'parsing a grammar with no TOP names TOP, not the outer .parse call';

throws-like { TOPlessGrammar.parse("a", :rule<y>) }, X::Method::NotFound,
    message => /'No such method \'y\' for invocant of type \'TOPlessGrammar\''/,
    ':rule<y> on an undefined rule names the requested rule';

grammar HasTop {
    token TOP { a }
}
is HasTop.parse("a").Str, 'a', 'a grammar that does define TOP still parses normally';
