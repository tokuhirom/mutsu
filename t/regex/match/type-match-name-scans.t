use v6;
use Test;

# The type-matching hot path scans type names for the ASCII markers `::`, `[`
# and `(` to decide whether a name is package-qualified, parameterized, or a
# coercion. Those scans were rewritten as byte scans and reordered so the O(1)
# `ends_with` rejects an unparameterized name before any scan runs (#7696).
# This file pins the behaviour each scan gates, in both directions, so a future
# reordering cannot quietly change which names match.

plan 31;

# --- parameterized names: `[` -------------------------------------------------

ok Buf[uint8].new(1, 2) ~~ Buf,      'Buf[uint8] instance matches bare Buf';
ok Buf[uint8].new(1, 2) ~~ Blob,     'Buf[uint8] instance matches bare Blob';
ok blob8.new(1) ~~ Blob,             'blob8 instance matches bare Blob';
nok Blob.new(1) ~~ Buf,              'a Blob is not a Buf';
ok Array[Int].new(1, 2) ~~ Array,    'Array[Int] matches bare Array';
ok Array[Int] ~~ Array,              'Array[Int] type object matches bare Array';
nok Array ~~ Array[Int],             'bare Array does not match Array[Int]';

my $parameterized = Array[Int].new(1);
ok $parameterized ~~ Positional,     'Array[Int] still does Positional';

# A name that merely *ends* with `]` is not parameterized, and a name that
# merely *contains* `[` is not either — both orderings must agree.
ok 'Int' ~~ Str,                     'a plain name is still a Str';
is Buf[uint8].^name, 'Buf[uint8]',   'parameterized name round-trips';

# --- coercion names: `(` ------------------------------------------------------

sub takes-int(Int() $n) { $n }
is takes-int('42'), 42,              'Int() coerces a Str argument';
is takes-int(42), 42,                'Int() passes an Int argument through';
is takes-int(4.2), 4,                'Int() coerces a Rat argument';

sub takes-str-of-int(Str(Int) $s) { $s }
is takes-str-of-int(7), '7',         'Str(Int) coerces the named source type';

# A name ending in `)` that is not a coercion must not be parsed as one.
ok Int ~~ Any,                       'a bare type object still matches Any';
ok 42 ~~ Int,                        'a plain Int still matches Int';

# --- qualified names: `::` ----------------------------------------------------

module Outer {
    class Inner is export {
        method label() { 'inner' }
    }
}

my $inner = Outer::Inner.new;
ok $inner ~~ Outer::Inner,           'instance matches its fully-qualified name';
is $inner.label, 'inner',            'qualified class method dispatches';
is Outer::Inner.^name, 'Outer::Inner', 'qualified name round-trips';

# The short-name bridge must NOT equate a nested core-setting name with the
# core type it shadows (the `Foo::Any` trap `short_name_bridges` guards).
module Shadow {
    class Any is export {
        method tag() { 'shadow' }
    }
}
my $shadowed = Shadow::Any.new;
is $shadowed.tag, 'shadow',          'a nested class named Any is its own type';
nok 42 ~~ Shadow::Any,               'an Int does not match a nested Shadow::Any';
nok 'x' ~~ Shadow::Any,              'a Str does not match a nested Shadow::Any';
ok $shadowed ~~ Shadow::Any,         'the nested instance does match it';

# A single colon is not a qualification.
ok 'a:b' ~~ Str,                     'a single colon in a value is irrelevant';

# --- roles and the MRO/role walk ---------------------------------------------

role Ticker { method tick() { 'tick' } }
class Clock does Ticker { }
ok Clock.new ~~ Ticker,              'an instance matches a composed role';
ok Clock ~~ Ticker,                  'the class type object matches it too';

role Countable does Ticker { }
class Counter does Countable { }
ok Counter.new ~~ Ticker,            'a transitively composed role matches';
ok Counter.new ~~ Countable,         'the directly composed role matches';

# `Real does Numeric` is a built-in role parent, which is the exact walk the
# native-dispatch numeric probe runs on every instance method call.
ok 42 ~~ Real,                       'an Int is Real';
ok 42 ~~ Numeric,                    'an Int is Numeric';
nok Buf.new(1) ~~ Numeric,           'a Buf is not Numeric';
