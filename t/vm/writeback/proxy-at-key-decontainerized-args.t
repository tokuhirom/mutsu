use Test;

# From the Printing::Jdf distribution: XML::Element's `AT-KEY` is `is rw` and
# answers a Proxy. A Proxy handed to a native constructor, or tested for
# truth, must be FETCHed like any other decontainerized read.

plan 8;

class E {
    has %.a;
    method AT-KEY($k) is rw {
        my $s = self;
        Proxy.new(FETCH => method () { $s.a{$k} }, STORE => method ($v) { $s.a{$k} = $v })
    }
}
my $e = E.new(a => { ts => "2014-07-02T04:55:31+12:45", path => "a/b.pdf", on => 1 });

is DateTime.new($e<ts>).Str, "2014-07-02T04:55:31+12:45", 'DateTime.new(Proxy) fetches';
is Date.new($e<ts>.substr(0, 10)).Str, "2014-07-02", 'Date.new(Str) baseline';
is IO::Path.new($e<path>).basename, "b.pdf", 'IO::Path.new(Proxy) fetches';

ok !(not $e<on>), 'not on a truthy Proxy is False';
ok (not $e<nope>), 'not on a falsy Proxy is True';
is (?$e<nope>), False, '? on a falsy Proxy';
is ($e<nope> ?? 1 !! 2), 2, 'ternary on a falsy Proxy';
is (so $e<on>), True, 'so on a truthy Proxy';
