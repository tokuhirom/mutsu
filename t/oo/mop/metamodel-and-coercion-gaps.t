use Test;

plan 12;

# `.^lookup` / `.^find_method` on a concrete value used to find nothing: the invocant's
# class name was flattened to "Any" before the registry was consulted, so only the type
# object worked.
class C { method v (Int $x --> Str()) { $x } }
my $o = C.new;

ok $o.^lookup('v').defined, '.^lookup on an instance finds the method';
ok $o.^find_method('v').defined, '.^find_method on an instance finds the method';
ok C.^find_method('v').defined, '.^find_method on the type object is unchanged';

# A coercion type's empty source is `Any`, which is the name `.returns` reports.
my sub arrow (Int $x --> Str()) { $x }
is &arrow.returns.raku, 'Str(Any)', '--> Str() reports Str(Any)';
my sub trait-form (Int $x) returns Str() { $x }
is &trait-form.returns.raku, 'Str(Any)', 'returns Str() reports Str(Any)';
is $o.^find_method('v').returns.raku, 'Str(Any)',
    "a method's declared coercion return type survives ^find_method";

# `.^find_method` still takes its adverbs: the name is the last *positional* argument.
ok C.^find_method('v', :no_fallback).defined, '.^find_method accepts :no_fallback';

# `Date`/`DateTime` take a `Dateish:D` invocant for `.IO`, so the type object throws
# rather than stringifying "(Date)" into a path.
is Date.new('2016-10-03').IO.Str, '2016-10-03', '.IO on a Date instance is its path';
throws-like { Date.IO }, X::Parameter::InvalidConcreteness, '.IO on Date:U throws';
throws-like { DateTime.IO }, X::Parameter::InvalidConcreteness, '.IO on DateTime:U throws';

# A scalar on the non-dwimmy side of a hyper cannot grow to meet the other side.
throws-like { my $ = 5 »*» (2..4) }, X::HyperOp::NonDWIM,
    'a scalar on the non-dwimmy side of a hyper throws';
is-deeply (5 «*» (2..4)).List, (10, 15, 20), 'the same scalar on the dwimmy side broadcasts';
