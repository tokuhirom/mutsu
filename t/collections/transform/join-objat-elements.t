use Test;

# `.join` over `ObjAt` / `ValueObjAt` elements (a `>>.WHICH` result) renders
# each as its identity text; a subclass with its own `Str` still runs it.

plan 6;

is (ValueObjAt.new("a"), ObjAt.new("b"), 1).join("|"), "a|b|1", 'mixed list';
is (ValueObjAt.new("a"), ObjAt.new("b")).Str, "a b", '.Str of the list';
is [ValueObjAt.new("a")].gist, "[a]", '.gist of an Array';

class V { has $.v; method WHICH { ValueObjAt.new("V|$!v") } }
my @a = V.new(:v(1)), V.new(:v(2));
is @a>>.WHICH.join("|"), "V|1|V|2", 'a hyper .WHICH result';
is (1, "x")>>.WHICH.join(","), "Int|1,Str|x", 'built-in identities';

role R { method Str { "role" } }
is (ValueObjAt.new("q") but R, 1).join("|"), "role|1", 'a role mixin keeps its own Str';
