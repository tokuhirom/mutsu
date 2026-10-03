use Test;

# A quoted regex term holds its decoded text, as rakudo 2026.09's
# `Regex::Quote` around a `StrLiteral` does, and a `StrLiteral` renders
# through `Str.raku`'s escaping.

plan 12;

sub quoted-text($src) {
    $src.AST.statements.head.expression.body.quoted.segments.head.value
}

is quoted-text(Q|/"x\ny"/|), "x\ny", 'a qq escape is decoded';
is quoted-text(Q|/"\x20"/|), ' ', 'a codepoint escape is decoded';
is quoted-text(Q|/"\x[41,42]"/|), 'AB', 'a codepoint list is decoded';
is quoted-text(Q|/'a\'b'/|), "a'b", 'a q string unescapes its quote';
is quoted-text(Q|/'\b'/|), '\b', 'and keeps any other backslash';
is quoted-text(Q|/‘a%b’/|), 'a%b', 'curly single quotes';
is quoted-text(Q|/｢a\b｣/|), 'a\b', 'corner brackets have no escapes';
is quoted-text(Q|/"x &f y"/|), 'x &f y', 'a `&name` without a call does not interpolate';

ok "x\ny" ~~ EVAL(Q|/"x\ny"/|.AST), 'a decoded escape survives the round trip';
ok 'q$' ~~ EVAL(Q|/"q\$"/|.AST), 'an escaped sigil is not interpolated after it';

is RakuAST::StrLiteral.new("x\ny\t").gist, 'RakuAST::StrLiteral.new("x\ny\t")',
    'a StrLiteral renders its control characters escaped';
is RakuAST::StrLiteral.new('a{b}').gist, 'RakuAST::StrLiteral.new("a\{b}")',
    'and its opening brace';
