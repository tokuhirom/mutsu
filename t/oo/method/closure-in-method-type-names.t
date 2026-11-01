use Test;
plan 8;

# Closure creation inside a method and the type names of callables share the
# well-known symbols the hot paths now use instead of re-interning them (#15530).
class C {
    has $.v = 5;
    method make { my $y = $!v; my $b = { $y + 1 }; $b }
    method counter { my $n = 0; -> { ++$n } }
}
my $o = C.new;
is $o.make.(), 6, 'a block created in a method sees the method lexical';
my $c = $o.counter;
$c(); $c();
is $c(), 3, 'a closure created in a method keeps mutating its capture';
is $o.make.WHAT.^name, 'Block', 'a block made in a method is a Block';
is (-> $a { $a }).WHAT.^name, 'Block', 'a pointy block is a Block';
is (* + 1).WHAT.^name, 'WhateverCode', 'a WhateverCode reports its type';
is C.^find_method('make').WHAT.^name, 'Method', 'a method object reports Method';
is (sub { 1 }).WHAT.^name, 'Sub', 'an anonymous sub is a Sub';
is $o.make.^name, 'Block', '.^name of a method-made block';
