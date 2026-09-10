use Test;

plan 7;

ok "(hello)" ~~ / "(" ~ ")" \w+ /, "tilde matches content up to closing delimiter";
nok "(hello" ~~ / "(" ~ ")" \w+ /, "tilde fails cleanly when closing delimiter is missing";

"abc" ~~ /a ~ (c) (b)/;
is ~$0, "c", "goal capture keeps source order";
is ~$1, "b", "inner capture stays after goal capture";

my regex recursive { "(" ~ ")" [ "a"* <&recursive>* ] }
ok "(()())" ~~ /^ <&recursive> $/, "tilde supports recursive balanced matching";
nok "(()" ~~ /^ <&recursive> $/, "recursive tilde rejects unterminated input";

throws-like { EVAL q{/ "[" ~? "]" /} }, X::Syntax::Regex::SolitaryQuantifier,
    "quantifying the tilde metachar reports the right syntax error";
