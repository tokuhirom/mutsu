use v6;
use nqp;
use Test;

# A byte-addressed read past the end of a buffer (`nqp::readuint`/`readint`/
# `readnum`, and the `.read-uint*`/`.read-int*`/`.read-num*` Buf/Blob methods
# that are implemented in terms of the same underlying op) must raise the
# exact MoarVM wording ("MVMArray: read_buf out of bounds ..."), because
# MoarVM-op-based decoders match it BY PREFIX in their own
# `CATCH { when /^ 'MVMArray: read_buf out of bounds' / { ... } }` to turn a
# truncated-input read into their own typed exception — CBOR::Simple's own
# upstream test suite (`02-malformed.rakutest`) depends on this for every
# "end of input" / truncated-data assertion. mutsu previously raised three
# DIFFERENT, non-matching messages across `nqp::readuint`/`readnum` and the
# `.read-uint16`-style methods, so none of them ever satisfied that `when`.

plan 5;

my $prefix = rx/^ 'MVMArray: read_buf out of bounds' /;

# nqp::readuint / nqp::readint
my $buf1 := buf8.new(1);
try nqp::readuint($buf1, 1, nqp::const::BINARY_SIZE_8_BIT +| NativeEndian);
like $!.Str, $prefix, 'nqp::readuint past-end message matches the MoarVM prefix';

# nqp::readnum
$! = Nil;
try nqp::readnum($buf1, 0, nqp::const::BINARY_SIZE_32_BIT +| NativeEndian);
like $!.Str, $prefix, 'nqp::readnum past-end message matches the MoarVM prefix';

# .read-uint16 (Buf/Blob method, a separate code path from the nqp:: ops)
$! = Nil;
try $buf1.read-uint16(0, BigEndian);
like $!.Str, $prefix, '.read-uint16 past-end message matches the MoarVM prefix';

# .read-num64
$! = Nil;
try $buf1.read-num64(0, BigEndian);
like $!.Str, $prefix, '.read-num64 past-end message matches the MoarVM prefix';

# The exact idiom CBOR::Simple's own test suite relies on: a CATCH block
# that regex-matches the message prefix to reclassify the exception.
my $reclassified;
{
    CATCH {
        when /^ 'MVMArray: read_buf out of bounds' / {
            $reclassified = 'caught-as-malformed';
        }
    }
    nqp::readuint($buf1, 1, nqp::const::BINARY_SIZE_8_BIT +| NativeEndian);
}
is $reclassified, 'caught-as-malformed',
    'a CATCH/when matching the MoarVM prefix reclassifies a truncated read';
