use Test;
plan 16;

# A user override of an `Any` interpreter row's method defers to the built-in
# with `callsame`/`nextsame` (ADR-11276 §9.60, #12388). Checked against raku.
class MyArr is Array {
    method map(|c)        { "map:"        ~ callsame().join(",") }
    method grep(|c)       { "grep:"       ~ callsame().join(",") }
    method first(|c)      { "first:"      ~ callsame() }
    method reduce(|c)     { "reduce:"     ~ callsame() }
    method produce(|c)    { "produce:"    ~ callsame().join(",") }
    method rotor(|c)      { "rotor:"      ~ callsame().map(*.join("")).join(",") }
    method skip(|c)       { "skip:"       ~ callsame().join(",") }
    method squish(|c)     { "squish:"     ~ callsame().join(",") }
    method classify(|c)   { "classify:"   ~ callsame().keys.sort.join(",") }
    method categorize(|c) { "categorize:" ~ callsame().keys.sort.join(",") }
    method match(|c)      { "match:"      ~ callsame() }
    method eager(|c)      { "eager:"      ~ callsame().join(",") }
}
my $a = MyArr.new(1, 2, 2, 3);
is $a.map(* * 2), "map:2,4,4,6", 'map';
is $a.grep(* > 1), "grep:2,2,3", 'grep';
is $a.first(* > 1), "first:2", 'first';
is $a.reduce(&[+]), "reduce:8", 'reduce';
is $a.produce(&[+]), "produce:1,3,5,8", 'produce';
is $a.rotor(2), "rotor:12,23", 'rotor';
is $a.skip(1), "skip:2,2,3", 'skip';
is $a.squish, "squish:1,2,3", 'squish';
is $a.classify(* %% 2), "classify:False,True", 'classify';
is $a.categorize({ $_ %% 2 ?? "even" !! "odd" }), "categorize:even,odd", 'categorize';
is $a.match(/2/), "match:2", 'match';
is $a.eager, "eager:1,2,2,3", 'eager';

# `nextsame` does not come back: the built-in's answer is the call's.
class N is Array { method map(|c) { "n:" ~ nextsame() } }
is-deeply N.new(1, 2).map(* + 1), (2, 3).Seq, 'nextsame into map';

# The built-ins are visible to `.^can`.
ok Any.^can('map'), 'Any.^can("map")';
ok (1, 2).^can('grep'), 'List.^can("grep")';
ok Any.^can('splice'), 'Any.^can("splice")';
