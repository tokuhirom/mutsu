use Test;

# A user class deriving from `Capture` (EEL's `Event is Capture`) keeps the
# `:list` / `:hash` its constructor is given, as rakudo's `Capture.new` does, and
# answers `.list`, `.hash`, `<key>` and `[i]` from them -- also through `$/`.

plan 13;

class Ev is Capture is Any {
    has $.x;
    method via-slash { $/ = self; $<p> }
    method positional-via-slash { $/ = self; $0 }
}

my $e = Ev.new(:hash({p => 'c'}), :list([1, 2]), :x(3));
is $e.x, 3, 'its own attribute';
is $e.list, (1, 2), '.list';
is $e.hash, Map.new((p => 'c')), '.hash';
is $e<p>, 'c', 'associative subscript';
is $e[1], 2, 'positional subscript';
is $e.elems, 2, '.elems';
ok $e<p>:exists, ':exists';
is $e.via-slash, 'c', '$<p> with $/ bound to the instance';
my $empty = Ev.new;
is $empty.hash.elems, 0, 'no :hash is an empty hash';
isa-ok $e, Capture, 'it is a Capture';
is $e.positional-via-slash, 1, '$0 with $/ bound to the instance';
{
    $/ = \(7, :b(2));
    is $0, 7, '$0 with $/ bound to a Capture';
    is $<b>, 2, '$<b> with $/ bound to a Capture';
}
