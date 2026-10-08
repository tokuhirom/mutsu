use Test;

# Attribute.build: `Mu` for an attribute with no initializer, the build block
# once `set_build` installed one. A typed `has Int $.id` must not report the
# parser's type-object seed as a build closure (Red's `next if $built =:= Mu`,
# RedX::HashedPassword).
plan 6;

class A { has Int $.id; has $.plain; has $.y = 5; }
my %attr = A.^attributes.map({ .name => $_ });
ok %attr<$!id>.build =:= Mu, 'typed attribute without initializer: build is Mu';
ok %attr<$!plain>.build =:= Mu, 'untyped attribute without initializer: build is Mu';
isa-ok %attr<$!y>.build, Int, 'literal initializer is reported';

my $at = Attribute.new(:name<$!z>, :package(A), :type(Any), :!has_accessor);
ok $at.build =:= Mu, 'fresh Attribute: build is Mu';
$at.set_build(sub (|) { 1 });
isa-ok $at.build, Sub, 'set_build is reflected by .build';
is $at.build.(), 1, 'the installed block is the one returned';
