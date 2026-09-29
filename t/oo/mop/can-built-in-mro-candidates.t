use Test;

# #9869: `.^can` returns one candidate per class in the MRO that declares
# the method (Rakudo's per-class `.^method_table`), most-derived first --
# built-in types included, not just user classes.

plan 13;

is Str.^can("uc").elems, 2, 'Str.^can("uc"): declared on Str and on Cool';
is "a".HOW.can("a".HOW, "uc").elems, 2, 'HOW.can form agrees';
is Int.^can("Str").elems, 2, 'Int.^can("Str"): Int and Mu, not Cool or Any';
is 42.^can("Str").elems, 2, 'instance receiver walks the same MRO';
is Bool.^can("Str").elems, 3, 'Bool.^can("Str"): Bool, Int and Mu';
is Sub.^can("name").elems, 1, 'Sub.^can("name"): only Code declares it';
is Str.^can("no-such-method").elems, 0, 'an unknown name has no candidates';

is Str.^can("uc")[0]("abc"), "ABC", 'the most-derived candidate is callable';
is Str.^can("uc")[1]("abc"), "ABC", 'the inherited candidate is callable';

class A { method m {} }
class B is A { method m {} }
is B.^can("m").elems, 2, 'user MRO: one candidate per declaring class';
is B.new.^can("gist").elems, 1, 'user class inherits the single Mu gist';

class C { has $.Str }
is C.^can("Str").elems, 2, 'attribute accessor comes before Mu.Str';
is C.^can("Str").map(*.package.^name).join(","), "C,Mu",
    'the accessor candidate is first';
