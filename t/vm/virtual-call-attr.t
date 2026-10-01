use Test;

# A virtual accessor call (`$.y`) in an attribute initializer dereferences the
# partially-constructed invocant and is a compile-time X::Syntax::VirtualCall.

plan 12;

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

# Every closure that keeps the invocant is checked -- a pointy block, a `sub`,
# a nested block, `do`, a WhateverCode -- as rakudo does; a method rebinds it.
throws-like 'class { has $.y; has $.x = -> { $.y } }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.y; has $.x = sub { $.y } }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.y; has $.x = { if True { $.y } } }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.y; has $.x = do { $.y } }', X::Syntax::VirtualCall, call => '$.y';
throws-like 'class { has $.y; has $.x = (1, 2).map(* + $.y) }', X::Syntax::VirtualCall, call => '$.y';
is (class F { has $.y = 7; has &.m = method { $.y } }).new.m.(F.new), 7,
    'an anonymous method in an initializer rebinds the invocant';
