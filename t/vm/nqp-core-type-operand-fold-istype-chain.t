# ADR-0115's fold list covers the type checks a dynamic dispatcher makes:
# JSON::Fast's `jsonify` asks `nqp::istype($_, Bool)`, then `IntStr`,
# `RatStr`, `NumStr`, `Enumeration`, `Str`, ... `Associative`, `Positional`,
# `Dateish`, `Instant`, `Real`, `Version` of every value it writes. Each of
# those operands folds to its CORE type object at parse time; this checks
# every folded name against every kind of value, with the expected bits taken
# from rakudo 2026.11, and that a unit binding one of the names still means
# its own binding.
use nqp;
use Test;

plan 3;

my @vals := (True, IntStr.new(1,"1"), RatStr.new(1.5,"1.5"), NumStr.new(1e0,"1e0"), ComplexStr.new(1i,"1i"), Order::Less, "str", 3, 1.5, FatRat.new(1,3), 1e0, 1i, (1,2).Seq, (1,2), {a=>1}, [1], Date.today, DateTime.now, now, Duration.new(1), Version.new("1.2"), Any, Bool, Int, sub {}, {;}, set(1), bag(1), mix(1), Pair.new(1,2), 1..2, Mu, Failure.new, IterationBuffer.new, "a".NFD, Exception.new, Buf.new, Blob.new, Slip.new);
my @got = @vals.map(-> \v {
    v.^name ~ ' ' ~ (nqp::istype(v, Any), nqp::istype(v, Array), nqp::istype(v, Associative), nqp::istype(v, Bag), nqp::istype(v, Blob), nqp::istype(v, Block), nqp::istype(v, Bool), nqp::istype(v, Buf), nqp::istype(v, Callable), nqp::istype(v, Capture), nqp::istype(v, Code), nqp::istype(v, Complex), nqp::istype(v, ComplexStr), nqp::istype(v, Cool), nqp::istype(v, Date), nqp::istype(v, DateTime), nqp::istype(v, Dateish), nqp::istype(v, Duration), nqp::istype(v, Enumeration), nqp::istype(v, Exception), nqp::istype(v, Failure), nqp::istype(v, FatRat), nqp::istype(v, Hash), nqp::istype(v, Instant), nqp::istype(v, Int), nqp::istype(v, IntStr), nqp::istype(v, Iterable), nqp::istype(v, IterationBuffer), nqp::istype(v, List), nqp::istype(v, Map), nqp::istype(v, Match), nqp::istype(v, Mix), nqp::istype(v, Mu), nqp::istype(v, NFC), nqp::istype(v, NFD), nqp::istype(v, NFKC), nqp::istype(v, NFKD), nqp::istype(v, Num), nqp::istype(v, NumStr), nqp::istype(v, Numeric), nqp::istype(v, Pair), nqp::istype(v, Positional), nqp::istype(v, Range), nqp::istype(v, Rat), nqp::istype(v, RatStr), nqp::istype(v, Rational), nqp::istype(v, Real), nqp::istype(v, Scalar), nqp::istype(v, Seq), nqp::istype(v, Set), nqp::istype(v, Slip), nqp::istype(v, Str), nqp::istype(v, Stringy), nqp::istype(v, Sub), nqp::istype(v, Uni), nqp::istype(v, Version)).map(+*).join
});
is-deeply @got.List, (
    'Bool 10000010000001000000000010000000100000010000001000000000',
    'IntStr 10000000000001000000000011000000100000010000001000011000',
    'RatStr 10000000000001000000000000000000100000010001111000011000',
    'NumStr 10000000000001000000000000000000100001110000001000011000',
    'ComplexStr 10000000000111000000000000000000100000010000000000011000',
    'Order 10000000000001000010000010000000100000010000001000000000',
    'Str 10000000000001000000000000000000100000000000000000011000',
    'Int 10000000000001000000000010000000100000010000001000000000',
    'Rat 10000000000001000000000000000000100000010001011000000000',
    'FatRat 10000000000001000000010000000000100000010000011000000000',
    'Num 10000000000001000000000000000000100001010000001000000000',
    'Complex 10000000000101000000000000000000100000010000000000000000',
    'Seq 10000000000001000000000000100000100000000000000010000000',
    'List 10000000000001000000000000101000100000000100000000000000',
    'Hash 10100000000001000000001000100100100000000000000000000000',
    'Array 11000000000001000000000000101000100000000100000000000000',
    'Date 10000000000000101000000000000000100000000000000000000000',
    'DateTime 10000000000000011000000000000000100000000000000000000000',
    'Instant 10000000000001000000000100000000100000010000001000000000',
    'Duration 10000000000001000100000000000000100000010000001000000000',
    'Version 10000000000000000000000000000000100000000000000000000001',
    'Any 10000000000000000000000000000000100000000000000000000000',
    'Bool 10000010000001000000000010000000100000010000001000000000',
    'Int 10000000000001000000000010000000100000010000001000000000',
    'Sub 10000100101000000000000000000000100000000000000000000100',
    'Block 10000100101000000000000000000000100000000000000000000000',
    'Set 10100000000000000000000000000000100000000000000001000000',
    'Bag 10110000000000000000000000000000100000000000000000000000',
    'Mix 10100000000000000000000000000001100000000000000000000000',
    'Pair 10100000000000000000000000000000100000001000000000000000',
    'Range 10000000000001000000000000100000100000000110000000000000',
    'Mu 00000000000000000000000000000000100000000000000000000000',
    'Failure 10000000000001000000100000000000100000000000000000000000',
    'IterationBuffer 10000000000000000000000000010000100000000000000000000000',
    'NFD 10000000000000000000000000000000101000000100000000001010',
    'Exception 10000000000000000001000000000000100000000000000000000000',
    'Buf 10001001000000000000000000000000100000000100000000001000',
    'Blob 10001000000000000000000000000000100000000100000000001000',
), 'every folded CORE type name answers nqp::istype as rakudo does';

sub by-param(\Bool) { nqp::istype(1, Bool) }
ok by-param(Int), 'a sigilless parameter spelled Bool is the parameter, not CORE Bool';

sub by-block() { my \Associative = Int; nqp::istype(1, Associative) }
ok by-block(), 'a nested sigilless binding shadows a CORE role name';
