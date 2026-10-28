use Test;

# A `where` on a multi method's own invocant parameter picks the candidate
# (Red's ColumnMethods: `multi method elems(\SELF where { .returns ~~ Json }:)`
# next to a candidate for every other column type).

plan 10;

class Col {
    has $.returns;
    multi method a($self where { .returns ~~ Str }:) { "str" }
    multi method a($self where { .returns !~~ Str }:) { "other" }
    multi method b(Col:D $self where { .returns ~~ Str }:) { "str" }
    multi method b(Col:D $self where { .returns !~~ Str }:) { "other" }
    multi method c(\SELF where { .returns ~~ Str }:) { "str" }
    multi method c(\SELF where { .returns !~~ Str }:) { "other" }
    multi method d(Col:D \SELF where { .returns ~~ Str }:) { "str" }
    multi method d(Col:D \SELF where { .returns !~~ Str }:) { "other" }
}

for <a b c d> -> $m {
    is Col.new(returns => Str)."$m"(), "str", "$m: the matching where picks its candidate";
    is Col.new(returns => Int)."$m"(), "other", "$m: the other where picks the other one";
}

role Meths {
    multi method size(\SELF where { .returns ~~ Str }:) { "str" }
    multi method size(\SELF where { .returns !~~ Str }:) { "other" }
}
class Col2 does Meths { has $.returns }
is Col2.new(returns => Str).size, "str", 'a composed role candidate: first where';
is Col2.new(returns => Int).size, "other", 'a composed role candidate: second where';
