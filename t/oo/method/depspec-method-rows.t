use Test;

# ADR-11276 §9.115: a `CompUnit::DependencySpecification` is one instance
# (no native value kind), and its methods are rows of the class.

plan 11;

my $d = CompUnit::DependencySpecification.new(:short-name<Foo>);
is $d.^name, 'CompUnit::DependencySpecification', '.^name of a plain depspec';
isa-ok $d, CompUnit::DependencySpecification, 'a plain depspec is an instance';
is $d.short-name, 'Foo', '.short-name';
is $d.version-matcher, True, 'an unset matcher matches anything';
is $d.from, 'Perl6', '.from defaults to Perl6';
is $d.Str, 'Foo', '.Str of a plain depspec';
is $d.raku, 'CompUnit::DependencySpecification.new(:short-name<Foo>)', '.raku';

my $m = CompUnit::DependencySpecification.new(
    :short-name<Foo>, :version-matcher<1.0+>, :auth-matcher<zef:x>);
is $m.Str, 'Foo:ver<1.0+>:auth<zef:x>', '.Str names the given matchers';
is $m.auth-matcher, 'zef:x', '.auth-matcher';
is $m.raku,
    'CompUnit::DependencySpecification.new(:short-name<Foo>,:version-matcher("1.0+"),:auth-matcher("zef:x"))',
    '.raku names the given matchers';
is $m.api-matcher, True, 'an unset api matcher';
