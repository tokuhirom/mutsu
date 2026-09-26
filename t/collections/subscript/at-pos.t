use Test;

plan 14;

class PositionalThing {
    method AT-POS($i) { "at($i)" }
    method EXISTS-POS($i) { $i == 7 || $i == 9 }
}

my $obj = PositionalThing.new;

is $obj[3], "at(3)", '[] dispatches to AT-POS';
ok $obj[7]:exists, ':exists dispatches to EXISTS-POS for existing index';
nok $obj[3]:exists, ':exists dispatches to EXISTS-POS for missing index';
is-deeply $obj[7, 3, 9]:exists, (True, False, True), 'slice :exists dispatches to EXISTS-POS per index';
is-deeply $obj[7, 3, 9]:exists:kv, (7, True, 9, True), ':exists:kv filters by actual existence';
is-deeply $obj[7, 3, 9]:exists:p, (7 => True, 9 => True), ':exists:p returns pairs for existing indices';
is-deeply $obj[7, 3, 9]:!exists, (False, True, False), ':!exists negates EXISTS-POS results';

# Bench 0.2.1 indexes the Bench::Time returned by timethis as `[0]`. An
# ordinary object inherits Any.AT-POS, which exposes the object as a one-element
# positional value when it defines neither AT-POS nor AT-KEY.
class PlainObject { }
my $plain = PlainObject.new;
is-deeply $plain[0], $plain, 'an ordinary instance is a one-element positional value';
nok $plain[1].defined, 'an ordinary instance is out of range past index zero';
is-deeply $plain.AT-POS(0), $plain, 'explicit AT-POS uses the inherited Any behavior';

class KeyedObject {
    method AT-KEY($key) { "key:$key" }
}
is KeyedObject.new[0], 'key:0', 'AT-KEY remains authoritative for positional object subscripts';

class PositionalWithoutATPOS does Positional {
    method elems { 2 }
}
my $positional-without-at-pos = PositionalWithoutATPOS.new;
is-deeply $positional-without-at-pos[0], $positional-without-at-pos,
    'a Positional instance inherits Any.AT-POS when it supplies none';
nok $positional-without-at-pos[1].defined,
    'the inherited Positional object is out of range past index zero';

class TypedArray is Array { }
my $typed-array = TypedArray[Int].new;
is $typed-array[0].WHAT, Int,
    'a typed Array subclass keeps its element default under AT-POS fallback';
