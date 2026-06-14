use v6;
use Test;

# `.gist` on a bare native-data instance (`Buf`/`Blob`/`Uni`) is pure
# formatting handled by `dispatch_core_repr`, yet the VM's "collection gist
# bypass" treated *any* Instance receiver — including a bare Buf — as a
# collection that might contain a user `method gist`, and forced it onto the
# interpreter. The bypass is now narrowed to actual collection receivers
# (list/array/hash); a bare instance dispatches natively, while a collection
# whose elements may carry a custom gist still defers (the builtins layer
# already does that itself). Byte-identical to the interpreter — these assert
# the rendered gist is unchanged and correct.

plan 9;

# --- Buf / Blob gist ---
is Buf.new(1, 2, 255).gist, 'Buf:0x<01 02 FF>', 'Buf.gist hex rendering';
is Blob.new(16, 32).gist, 'Blob:0x<10 20>', 'Blob.gist hex rendering';
is buf16.new(0x1234, 0xABCD).gist, 'Buf[uint16]:0x<1234 ABCD>',
    'buf16.gist uses 4-wide hex';

# --- Uni gist ---
is Uni.new(72, 105).gist, 'Uni:0x<0048 0069>', 'Uni.gist hex rendering';

# --- a collection that contains instances still gists its elements ---
my @bufs = Buf.new(1, 2), Buf.new(3, 4);
is @bufs.gist, '[Buf:0x<01 02> Buf:0x<03 04>]', 'a list of Bufs gists each element';

# --- a user instance with a custom gist still dispatches the method ---
class P { has $.x; method gist { "P<{$!x}>" } }
is P.new(x => 7).gist, 'P<7>', 'a user method gist is honored on a bare instance';

# --- a list mixing a custom-gist instance still honors it ---
my @mix = Buf.new(9), P.new(x => 1);
is @mix.gist, '[Buf:0x<09> P<1>]', 'a mixed list gists Buf natively and P via its method';

# --- a hash with Buf values ---
my %h = k => Buf.new(7, 8);
is %h.gist, '{k => Buf:0x<07 08>}', 'a hash gist renders a Buf value';

# --- gist of a Buf inside say still works end to end ---
is Buf.new(0xDE, 0xAD).gist, 'Buf:0x<DE AD>', 'Buf.gist uppercase hex';
