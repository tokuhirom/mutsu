use Test;

# ADR-11276 §9.94: `Complex.Complex`, `Any.serial` and the `Blob`/`Buf`
# reinterpretations are rows; the last arms of `dispatch_core_coerce.rs` are
# gone. The answers below are Rakudo's.

plan 14;

ok Complex.Complex === Complex, 'Complex.Complex is the type object';
is (1+2i).Complex, 1+2i, 'a Complex is its own Complex';

is 5.serial, 5, 'Int.serial is the receiver';
ok Int.serial === Int, 'a type object is its own serial';
is-deeply (1, 2).Seq.serial.list, (1, 2), 'Seq.serial';
is-deeply (1..2).serial, 1..2, 'Range.serial';
is (a => 1).serial, (a => 1), 'Pair.serial';
ok Any.serial === Any, 'Any.serial';
throws-like { Mu.serial }, X::Method::NotFound, 'Mu has no serial';

is "ab".encode.Buf.^name, 'Buf', 'utf8.Buf is a Buf';
is "\r\n".encode.Buf.list.join(','), '13,10', '... over the same bytes';
is Buf.new(1, 2).Blob.^name, 'Blob', 'Buf.Blob is a Blob';
throws-like { Blob.new(1, 2).Blob }, X::Method::NotFound, 'Blob declares no Blob';
throws-like { "ab".encode.Blob }, X::Method::NotFound, '... nor does utf8';
