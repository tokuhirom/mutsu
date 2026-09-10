use Test;

# A virtual accessor call (`$.y`) in an attribute initializer dereferences the
# partially-constructed invocant and is a compile-time X::Syntax::VirtualCall.

plan 6;

throws-like 'class { has $.x = $.y }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.x = $.y + 1 }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.x = { $.y } }', X::Syntax::VirtualCall, call => '$.y';

# Valid initializers still compile and run.
my $c = class C { has $.a = 1; has $.b = 2 }.new;
is $c.b, 2, 'literal attribute defaults work';

my $outer = 9;
is (class D { has $.x = $outer }).new.x, 9, 'closure-captured outer var default works';

# Direct attribute access ($!y) in an initializer is allowed.
lives-ok { (class E { has $.y; has $.x = $!y }).new(y => 3) }, '$!y direct access is allowed';
