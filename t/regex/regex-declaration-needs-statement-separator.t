use Test;

# #19030: a `token`/`rule`/`regex` declaration ends with its block, like a
# `sub` or `method`. A second statement on the same line without a `;` is
# "Strange text after block", as on rakudo.

plan 7;

for 'token a { x } token b { y }', 'rule a { x } regex b { y }',
    'regex a { x } token b { y }' -> $body {
    throws-like "grammar G \{ $body \}", X::Syntax::Confused,
        "two declarations on one line: $body";
}

lives-ok { EVAL 'grammar G1 { token a { x }; token b { y } }' }, 'a semicolon separates them';
lives-ok { EVAL "grammar G2 \{ token a \{ x }\n token b \{ y } }" }, 'so does a newline';
lives-ok { EVAL "grammar G3 \{ token a \{ x } # note\n }" }, 'a trailing comment is fine';
ok EVAL('grammar G4 { rule TOP { x } }; G4.parse("x")'), 'a lone rule still parses';
