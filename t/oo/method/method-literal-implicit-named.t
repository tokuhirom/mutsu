use Test;

class Receiver { }
my &mapped = method (@a) { @a.map({ $_ ~ %_.elems }).join(',') };
is Receiver.&mapped(<a b>, :x), 'a1,b1',
    'a nested closure reads the method literal leftover named arguments';

my &unused = method ($value) { $value };
is Receiver.&unused(42, :extra), 42, 'unused leftover named arguments are accepted';

my &consumed = method (:$used = 9) { $used ~ '/' ~ %_.keys.sort.join(',') };
is Receiver.&consumed(:used(7), :x, :z), '7/x,z', 'consumed named parameters are excluded';
is Receiver.&consumed(:x), '9/x', 'defaulted named parameters are excluded';

my &explicit = method (*%_) { %_.elems };
is Receiver.&explicit(:x, :y), 2, 'an explicit named catch-all is preserved';
my &other = method (*%rest) { %rest.keys.sort.join(',') };
is Receiver.&other(:x, :y), 'x,y', 'another explicit named catch-all is preserved';

my &tried = method () { try { %_.elems } };
is Receiver.&tried(:x), 1, 'a no-signature try block captures the method named arguments';

my &submapped = submethod (@a) { @a.map({ $_ ~ %_.elems }).join(',') };
is Receiver.&submapped(<a b>, :x), 'a1,b1', 'submethod literals get the same implicit named parameter';

my &signatureless = method { %_.elems };
is Receiver.&signatureless(:x), 1, 'a signatureless method literal has named arguments';
my &returning = method () {
    return %_.elems;
};
is Receiver.&returning(:x), 1, 'a multiline method literal remains a routine boundary';

my &escaping = method () { return { %_.elems } };
my &first = Receiver.&escaping(:x);
my &second = Receiver.&escaping(:x, :y);
is first(), 1, 'an escaped closure retains its own method invocation named hash';
is second(), 2, 'a second invocation has a separate named hash';

my &ordinary = sub () { 42 };
dies-ok { ordinary(:extra) }, 'an ordinary sub does not gain a method named catch-all';
my &pointy = -> () { 42 };
dies-ok { pointy(:extra) }, 'a pointy block does not gain a method named catch-all';
done-testing;
