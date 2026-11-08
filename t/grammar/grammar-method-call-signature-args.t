use v6.e.PREVIEW;
use Test;

# A grammar rule with a signature called as a method, with arguments:
# `self.thing('kw')`, `self."thing"('kw')`, `self."$name"('kw')`.

plan 6;

my @seen;
grammar G {
    token thing($d) { 'b' { @seen.push($d) } }
    method direct() { self.thing('direct') }
    method quoted() { self."thing"('quoted') }
    method dynamic($n) { self."$n"('dynamic') }
    token TOP { 'a' [ <x=.direct> | <x=.quoted> | <x=.dynamic('thing')> ] }
}

ok G.parse('ab'), 'a method delegating to a signature rule parses';
is @seen[0], 'direct', 'self.thing(...) passes the argument';

@seen = ();
grammar H {
    token thing($d) { 'b' { @seen.push($d) } }
    method quoted() { self."thing"('quoted') }
    token TOP { 'a' <x=.quoted> }
}
ok H.parse('ab'), 'the quoted method name works';
is @seen[0], 'quoted', 'self."thing"(...) passes the argument';

@seen = ();
grammar K {
    token thing($d) { 'b' { @seen.push($d) } }
    method dynamic($n) { self."$n"('dynamic') }
    token TOP { 'a' <x=.dynamic('thing')> }
}
ok K.parse('ab'), 'a computed method name works';
is @seen[0], 'dynamic', 'self."$name"(...) passes the argument';
