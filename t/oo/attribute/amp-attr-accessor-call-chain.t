use v6;
use Test;

plan 3;

# `&.name` is the public accessor of a `has &.name` callable attribute
# (`self.name`). A bare `()` immediately following it is the accessor's OWN
# (empty) argument list, not a second invocation of the callable it returns —
# only a call chained AFTER that first one invokes the stored callable.
# `&.cb()(|%args)` must therefore call `cb` exactly once, with `%args`.
class Holder {
    has &.cb is required;

    method run($data) {
        my %args;
        given &.cb.signature.params {
            %args<out>  = $data if .grep: *.name eq '$out';
            %args<data> = $data if .grep: *.name eq '$data';
        }
        &.cb()(|%args);
    }
}

my @calls;
my $h = Holder.new(cb => -> :$out, :$data { @calls.push: "out=$out data=$data" });
$h.run("hello");
is @calls.elems, 1, 'the stored callable ran exactly once';
is @calls[0], 'out=hello data=hello', 'it ran with the computed named args, not zero args';

# Two chained calls after a bare `&.name` term must still count separately:
# only the LAST call gets the real arguments; the first is the accessor call.
class Adder {
    has &.f is required;
    method go() { &.f()(3, 4) }
}
my $a = Adder.new(f => -> $x, $y { $x + $y });
is $a.go, 7, '&.attr()(args) invokes the fetched callable once with args';

done-testing;
