use Test;

# ADR-11276 §9.82: Mu's item and sink, Iterable's race and hyper and the
# succ/pred rows reach every receiver through the method table, without the
# name-matching arms of `methods_0arg/dispatch_core_math.rs`.

plan 5;

class Plain { }
enum E <ex why>;

subtest 'item reaches every receiver', {
    plan 7;
    is-deeply 5.item, 5, 'an Int';
    is-deeply Int.item, Int, 'a type object';
    is-deeply (a => 1).item, (a => 1), 'a Pair';
    isa-ok Plain.new.item, Plain, 'an instance';
    is-deeply why.item, why, 'an enum value';
    my $m = "ab" ~~ /a/;
    isa-ok $m.item, Match, 'a Match is itself, not its matched string';
    my @a = 1, 2;
    my $n = 0;
    $n++ for @a.item;
    is $n, 1, 'an itemized array iterates once';
}

subtest 'sink reaches every receiver', {
    plan 6;
    my $ran = 0;
    (gather { $ran++ }).sink;
    is $ran, 1, 'sinking a gather runs it';
    (1..3).map({ $ran++ }).sink;
    is $ran, 4, 'sinking a map runs it';
    my $f = sub { Failure.new("boom") }();
    $f.so;
    lives-ok { $f.sink }, 'a handled Failure sinks quietly';
    my $g = sub { Failure.new("boom2") }();
    throws-like { $g.sink }, Exception, message => 'boom2', 'an unhandled Failure throws';
    is-deeply Plain.new.sink, Nil, 'an instance sinks to Nil';
    is-deeply set(1).sink, Nil, 'a Set sinks to Nil';
}

subtest 'race and hyper reach the Iterable receivers only', {
    plan 5;
    is-deeply (1, 2, 3).race.map(* * 2).List.sort.List, (2, 4, 6), 'a List races';
    is-deeply (gather { take 1; take 2 }).hyper.List, (1, 2), 'a gather hypers';
    is-deeply (1..3).map(* + 1).race.List.sort.List, (2, 3, 4), 'a map races';
    throws-like { 5.race }, X::Method::NotFound, 'an Int has no race';
    throws-like { Plain.new.hyper }, X::Method::NotFound, 'an instance has no hyper';
}

subtest 'succ and pred', {
    plan 6;
    is-deeply Bool.succ, True, 'Bool.succ on the type object';
    is-deeply Bool.pred, False, 'Bool.pred on the type object';
    is-deeply "az".succ, "ba", 'a Str';
    is-deeply 1.5.pred, 0.5, 'a Rat';
    is-deeply why.pred, ex, 'an enum value';
    my $x = Int;
    $x++;
    is-deeply $x, 1, 'incrementing an undefined Int';
}

subtest 'Mu rows answer a Match on itself', {
    plan 2;
    my $m = "x" ~~ /<?before x>/;
    is-deeply $m.so, True, 'a zero-width match is true';
    is-deeply $m.not, False, 'and not False';
}
