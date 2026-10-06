use Test;

plan 7;

class MyP is IO::Path {}
my $q = MyP.new("abc/def");

is $q.uc, "ABC/DEF", "Cool .uc on an IO::Path subclass runs on the path";
ok $q.starts-with("a"), ".starts-with on a subclass";
nok $q.ends-with("a"), ".ends-with on a subclass";
is $q.chars, 7, ".chars on a subclass";
is $q.Str, "abc/def", ".Str still the path";
is $q.basename, "def", "IO::Path's own methods are untouched";

class Over is IO::Path { method uc { "mine" } }
is Over.new("x/y").uc, "mine", "a user override still wins";
