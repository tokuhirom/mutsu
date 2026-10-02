use Test;
# From Map::Match: a user postcircumfix candidate receives the built-in slice
# adverbs (:k :v :kv :p) as named arguments.
plan 6;

class Foo {
    method CALL-ME(\key, :$k, :$v, :$kv, :$p) {
        join ",", key.raku, $k.raku, $v.raku, $kv.raku, $p.raku
    }
}
multi sub postcircumfix:<{ }>(Foo:D $m, \keys, *%_) { $m.CALL-ME(keys, |%_) }

my $f = Foo.new;
is $f{"a"},     '"a",Any,Any,Any,Any',                'no adverb';
is $f{"a"}:k,   '"a",Bool::True,Any,Any,Any',         ':k';
is $f{"a"}:v,   '"a",Any,Bool::True,Any,Any',         ':v';
is $f{"a"}:p,   '"a",Any,Any,Any,Bool::True',         ':p';

my %h = a => 1, b => 2;
is-deeply %h<a>:k, "a", 'Hash target keeps the core :k';
is-deeply %h{"a"}:p, (a => 1), 'Hash target keeps the core :p';
