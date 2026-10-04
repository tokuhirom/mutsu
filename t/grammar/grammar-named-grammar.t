use Test;

# Template::HAML::Grammar declares `grammar Grammar is export`: the implicit
# core Grammar parent is spelled CORE::Grammar and must be a known parent.
plan 3;

grammar Grammar {
  token TOP { \w+ }
}

is Grammar.parse("abc").Str, "abc", "a grammar named Grammar parses";
is Grammar.^name, "Grammar", "it keeps its own name";
ok Grammar.^mro.map(*.^name).tail(3).join(",") eq "Cool,Any,Mu", "it still inherits Match";
