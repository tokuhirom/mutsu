use Test;

# ADR-11276 §9.81: Mu's so, not and is-lazy, Any's join and Iterable's lazy
# reach every receiver through the method table, without the name-matching
# cascade arms (`methods_0arg/dispatch_core_str.rs`) that used to answer them.

plan 6;

class Plain { }
class WithBool { method Bool { False } }
class MyCool is Cool { method Str { "mc" } }
class MyIntB is Int { method Bool { False } }
enum E <ex why>;

subtest 'so and not reach every receiver', {
    plan 14;
    is-deeply Int.so, False, 'a type object';
    is-deeply Plain.not, True, 'a user type object';
    is-deeply Plain.new.so, True, 'an instance';
    is-deeply WithBool.new.so, False, 'an instance with its own Bool';
    is-deeply WithBool.new.not, True, 'not asks the instance Bool';
    is-deeply MyIntB.new(5).so, False, 'a native-base instance with its own Bool';
    is-deeply (sub { }).so, True, 'a Sub';
    is-deeply (a => 1).so, True, 'a Pair';
    is-deeply set().not, True, 'an empty Set';
    is-deeply \(1).so, True, 'a Capture';
    is-deeply ("a" ~~ /b/).not, True, 'a failed match';
    is-deeply why.so, True, 'an enum value';
    is-deeply (gather { take 1 }).so, True, 'a gather Seq';
    is-deeply IO::Spec::Unix.so, False, 'a type-object-only shape';
}

subtest 'so and not handle a Failure', {
    plan 3;
    my $f = sub { Failure.new("boom") }();
    is-deeply $f.so, False, 'so is False';
    is-deeply $f.not, True, 'not is True';
    ok $f.handled, 'and the Failure is handled';
}

subtest 'is-lazy reaches every receiver', {
    plan 9;
    is-deeply 5.is-lazy, False, 'an Int';
    is-deeply Int.is-lazy, False, 'a type object';
    is-deeply Plain.new.is-lazy, False, 'an instance';
    is-deeply (sub { }).is-lazy, False, 'a Sub';
    is-deeply (a => 1).is-lazy, False, 'a Pair';
    is-deeply (1..*).is-lazy, True, 'an infinite Range';
    is-deeply (1..*).map(* + 1).is-lazy, True, 'a lazy map';
    is-deeply (gather { take 1 }).is-lazy, False, 'a plain gather is not lazy';
    is-deeply (1..*).iterator.is-lazy, True, 'a lazy Iterator';
}

subtest 'lazy reaches the Iterable receivers only', {
    plan 8;
    is-deeply Array.lazy[0], Array, 'an Array type object lists as itself';
    is-deeply Hash.lazy.is-lazy, True, 'so does a Hash type object';
    my @a = (1..3).map(* + 1).lazy;
    is-deeply @a.is-lazy, True, 'a lazy map list stays lazy when assigned';
    my $g = gather { take 1; take 2 };
    my @b = $g.lazy;
    is-deeply @b.is-lazy, True, 'a gather made lazy stays lazy when assigned';
    is-deeply (lazy gather { take 3 }).is-lazy, True, 'the lazy prefix';
    is-deeply (1, 2).Slip.lazy.is-lazy, True, 'a Slip';
    throws-like { 5.lazy }, X::Method::NotFound, 'an Int has no lazy';
    throws-like { Plain.new.lazy }, X::Method::NotFound, 'an instance has no lazy';
}

subtest 'join reads any receiver as its list', {
    plan 13;
    is-deeply "x".IO.join(","), "x", 'an IO::Path';
    is-deeply MyCool.new.join, "mc", 'an instance joins its own Str';
    is-deeply MyCool.new.join(","), "mc", 'with a separator';
    is-deeply Blob.new(1, 2).join(","), "1,2", 'a Blob joins its elements';
    is-deeply ("a" ~~ /a/).join, "", 'a Match joins its positional captures';
    is-deeply ("ab" ~~ /(a)(b)/).join(","), "a,b", 'with a separator';
    is-deeply (a => 1).join(","), "a\t1", 'a Pair';
    is-deeply \(1, 2).join(","), "1,2", 'a Capture';
    is-deeply set(1).join, "1\tTrue", 'a Set';
    is-deeply Version.new("1.2").join(","), "1.2", 'a Version';
    is-deeply why.join, "why", 'an enum value';
    is-deeply 5.join(","), "5", 'an Int';
    is-deeply (1..3).join, "123", 'a Range';
}

subtest 'join on a type object warns and is empty', {
    plan 4;
    my $warned = 0;
    {
        is-deeply Int.join, "", 'Int.join';
        is-deeply Plain.join(","), "", 'a user type object';
        CONTROL { when CX::Warn { $warned++; .resume } }
    }
    is $warned, 2, 'each warned';
    is-deeply Nil.join, "", 'Nil.join';
    CONTROL { when CX::Warn { .resume } }
}
