use Test;

# ADR-11276 §9.105: the `AT-POS`/`EXISTS-POS` rows of `Range`, `Uni`,
# `Capture` and `List` take an `Int`; every other index is bound the way
# Rakudo's candidates bind it (`method_table::index_arg`).

plan 30;

class P { }
class N { method Int { 1 } }
class I is Int { }
enum E <ea eb ec>;

my $uni = "abc".NFC;
my $cap = \(1, 2, 3);

# `Any.AT-POS(Any:D \pos)` re-dispatches on `pos.Int`, a user object's too.
is (1..5).AT-POS(N.new), 2, 'Range.AT-POS: a user object is read as its .Int';
is $uni.AT-POS(N.new), 98, 'Uni.AT-POS: a user object is read as its .Int';
is $cap.AT-POS(N.new), 2, 'Capture.AT-POS: a user object is read as its .Int';
is (1..5).AT-POS(1..2), 3, 'Range.AT-POS: a Range index is its .Int';
is $uni.AT-POS(1..2), 99, 'Uni.AT-POS: a Range index is its .Int';
is $cap.AT-POS(1..2), 3, 'Capture.AT-POS: a Range index is its .Int';
is $cap.AT-POS((1, 2)), 3, 'Capture.AT-POS: a List index is its .Int';
is $uni.AT-POS(1.5), 98, 'Uni.AT-POS: a Rat index truncates';
is $cap.AT-POS(<1>), 2, 'Capture.AT-POS: an allomorph index';
is (1..5).AT-POS("2"), 3, 'Range.AT-POS: a Str index';

# An `Int:D` binds the row's own candidate.
is (1..5).AT-POS(eb), 2, 'Range.AT-POS: an Int enum';
is $cap.AT-POS(True), 2, 'Capture.AT-POS: a Bool';
is (1..5).AT-POS(I.new(2)), 3, 'Range.AT-POS: an Int subclass instance';
ok (1..5).EXISTS-POS(True), 'Range.EXISTS-POS: a Bool is an Int';
ok (1..5).EXISTS-POS(I.new(2)), 'Range.EXISTS-POS: an Int subclass instance is an Int';

# `Any.EXISTS-POS(Any:D \pos)` is `pos.Int == 0`.
nok (1..5).EXISTS-POS(N.new), 'Range.EXISTS-POS: a user object is not Int:D';
nok $uni.EXISTS-POS(N.new), 'Uni.EXISTS-POS: a user object is not Int:D';
nok $uni.EXISTS-POS("1"), 'Uni.EXISTS-POS: a Str index is not Int:D';
ok (1..5).EXISTS-POS("0"), 'Range.EXISTS-POS: a Str "0" addresses Any\'s one element';
nok (1, 2).EXISTS-POS("1"), 'List.EXISTS-POS: a Str index is not Int:D';
nok [1, 2].EXISTS-POS(1.0), 'Array.EXISTS-POS: a Rat index is not Int:D';
nok (1..5).EXISTS-POS(1e0), 'Range.EXISTS-POS: a Num index is not Int:D';

# `Capture`'s index parameter is `Int(Any)`: everything is coerced.
ok $cap.EXISTS-POS(N.new), 'Capture.EXISTS-POS: a user object is coerced';
ok $cap.EXISTS-POS("1"), 'Capture.EXISTS-POS: a Str is coerced';
{
    my $warned = 0;
    my $answer = do {
        CONTROL { when CX::Warn { $warned++; .resume } }
        $cap.AT-POS(P);
    };
    is $answer, 1, 'Capture.AT-POS: a type object is coerced to 0';
    is $warned, 1, '... with the uninitialized-value warning';
}

# A type object is no index.
throws-like { (1..5).AT-POS(P) }, Exception, message => "Cannot use 'P' as an index",
    'Range.AT-POS: a type object';
throws-like { $uni.EXISTS-POS(Int) }, Exception, message => "Cannot use 'Int' as an index",
    'Uni.EXISTS-POS: a type object';

# A user object without `.Int` has no candidate.
throws-like { $cap.AT-POS(P.new) }, X::Multi::NoMatch, 'Capture.AT-POS: no .Int';
throws-like { $uni.AT-POS(P.new) }, X::Multi::NoMatch, 'Uni.AT-POS: no .Int';
