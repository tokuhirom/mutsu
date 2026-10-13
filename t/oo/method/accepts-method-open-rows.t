use Test;

# ADR-11276 §9.108: `ACCEPTS` of the open shapes, `Uni`, the buffers and
# `Any` are method-table rows. `Any.ACCEPTS` is `self === topic`, so a
# `Capture` or a plain instance accepts only itself; a `Uni` compares its
# codepoints, whatever the normalization form.

plan 37;

class P { }
class A { method ACCEPTS($x) { "A:$x" } }
role R { method ACCEPTS($x) { "R:$x" } }
class B does R { }

# The numeric shapes answer `Numeric`'s candidate.
ok 5.ACCEPTS(5.0), 'Int.ACCEPTS: a Rat topic of the same value';
ok 5.ACCEPTS("5"), 'Int.ACCEPTS: a numeric Str topic';
nok 5.ACCEPTS(Int), 'Int.ACCEPTS: a type object topic is False';
ok 5.0.ACCEPTS(5), 'Rat.ACCEPTS';
ok 5e0.ACCEPTS(5), 'Num.ACCEPTS';
ok 5.FatRat.ACCEPTS(5), 'FatRat.ACCEPTS (was X::Method::NotFound)';
nok Instant.from-posix(5).ACCEPTS(5), 'Instant.ACCEPTS compares its value';

# Str, Bool, List, Map, Date, DateTime, Seq.
ok "abc".ACCEPTS("abc"), 'Str.ACCEPTS';
nok "abc".ACCEPTS("abd"), 'Str.ACCEPTS: another string';
ok True.ACCEPTS(False), 'True.ACCEPTS is True';
nok False.ACCEPTS(True), 'False.ACCEPTS is False';
ok (1, 2).ACCEPTS([1, 2]), 'List.ACCEPTS: an Array topic';
nok (1, 2).ACCEPTS((1, *)), 'List.ACCEPTS: the receiver is the pattern';
ok [1, 2].ACCEPTS((1, 2)), 'Array.ACCEPTS';
ok {a => 1}.ACCEPTS("a"), 'Hash.ACCEPTS: a key that exists';
nok {a => 1}.ACCEPTS("b"), 'Hash.ACCEPTS: a key that does not';
ok Date.new(2020, 1, 1).ACCEPTS(Date.new(2020, 1, 1)), 'Date.ACCEPTS';
ok DateTime.new(2020, 1, 1, 0, 0, 0).ACCEPTS(DateTime.new(2020, 1, 1, 0, 0, 0)),
    'DateTime.ACCEPTS';
nok DateTime.new(2020, 1, 1, 0, 0, 0).ACCEPTS(5), 'DateTime.ACCEPTS: another type';
ok (1, 2).Seq.ACCEPTS((1, 2)), 'Seq.ACCEPTS';
my $m = 'ab' ~~ /a/;
ok $m.ACCEPTS(5) === $m, 'Match.ACCEPTS is the Match itself';

# Capture: `Any.ACCEPTS`, identity by value.
ok \(1).ACCEPTS(\(1)), 'Capture.ACCEPTS: an equal Capture';
nok \(1).ACCEPTS(1), 'Capture.ACCEPTS: not smartmatched against its contents';
ok \(1, :a(2)).ACCEPTS(\(1, :a(2))), 'Capture.ACCEPTS: named parts';
nok \(1).ACCEPTS(Capture), 'Capture.ACCEPTS: a type object topic';

# Uni: the codepoints.
ok "ab".NFC.ACCEPTS("ab".NFKC), 'Uni.ACCEPTS: another form, the same codepoints';
nok "é".NFD.ACCEPTS("é".NFC), 'Uni.ACCEPTS: different codepoints';
nok "abc".NFC.ACCEPTS("abc"), 'Uni.ACCEPTS: a Str topic is not a Uni';

# Buffers.
ok Blob.new(1, 2).ACCEPTS(Buf.new(1, 2)), 'Blob.ACCEPTS: the same elements';
ok Buf.new(1, 2).ACCEPTS(Blob.new(1, 2)), 'Buf.ACCEPTS';
nok Buf.new(1, 2).ACCEPTS(5), 'Buf.ACCEPTS: not a buffer';

# A user class: `Any.ACCEPTS`, unless it declares its own.
my $p = P.new;
ok $p.ACCEPTS($p), 'Any.ACCEPTS: the same instance';
nok $p.ACCEPTS(P.new), 'Any.ACCEPTS: another instance';
is A.new.ACCEPTS(3), 'A:3', 'a class method ACCEPTS wins';
is B.new.ACCEPTS(4), 'R:4', 'a role method ACCEPTS wins';

# A type object is `Mu.ACCEPTS`, the type check.
ok Int.ACCEPTS(5), 'Int.ACCEPTS: the type check';
is-deeply 5.ACCEPTS(1|5).Bool, True, 'a Junction topic autothreads';
