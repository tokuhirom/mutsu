use Test;

# `:name(alias) (sub-signature)`: the trailing sub-signature destructures the
# named argument's value, in addition to the alias binding (#10310,
# CSS::Stylesheet's `load(:at-rule(%)! (Str:D :at-keyw($type)!, *%ast))`).
plan 11;

sub f1(:x($r) ($a, $b)) { "$a-$b" }
is f1(x => (1, 2)), '1-2', 'positional destructure of a named value';

sub f2(:x(%h) (:$k!, *%rest)) { "$k|{%rest.keys}|{%h.elems}" }
is f2(x => {k => 5, z => 1}), '5|z|2', 'hash alias plus named destructure';

sub f3(:x($) (:$k!)) { "k=$k" }
is f3(x => {k => 5}), 'k=5', 'anonymous scalar alias';

sub f4(:at-rule(%)! (Str:D :at-keyw($type)!, *%ast)) { "$type {%ast.keys.sort}" }
is f4(at-rule => {:at-keyw<media>, :rule-list(1)}), 'media rule-list', 'required, anonymous hash alias, renamed leaf';
my %a = :at-rule({:at-keyw<media>, :x(1)});
is f4(|%a), 'media x', 'from a flattened hash';

class S {
    multi method at-rule('media', :@rule-list) { "media" }
    multi method at-rule($rule, |c) { "other" }
    multi method load(:at-rule(%)! (Str:D :at-keyw($type)!, *%ast), |c) {
        $.at-rule: $type, |%ast, |c;
    }
}
is S.new.load(:at-rule({:at-keyw<media>, :rule-list([2])})), 'media', 'multi method with the sub-signature';
is S.new.load(:at-rule({:at-keyw<page>})), 'other', 'other keyword';

dies-ok { f4(at-rule => {:x(1)}) }, 'a missing required leaf fails the bind';

# A value that does not unpack makes the candidate inapplicable.
multi pick(:q(@)! (% (:keyw($_)!), $m)) { "first" }
multi pick(:q(@)! ($m, *@e)) { "second" }
is pick(q => [{ident => 'print'},]), 'second', 'missing required key: next candidate';
is pick(q => [{keyw => 'x'}, 3]), 'first', 'fitting value: first candidate';

multi arity(:q(@)! ($m, $z)) { "two" }
multi arity(:q(@)! ($m, *@e)) { "slurpy" }
is arity(q => [1]), 'slurpy', 'too few elements';
