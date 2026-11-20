use Test;

# The ctor lane's early form answers `Class.new(named...)` at the top of
# `CallMethodMut`, before the argument stretch, once the class is in the lane
# (`src/vm/vm_ctor_early_lane.rs`). Each loop below constructs repeatedly, so
# the later iterations take the early form. Every argument shape the stretch
# would rewrite is refused there and still answers what the full dispatch
# answers.

plan 14;

my class Token {
    has Str  $.kind is required;
    has Str  $.name = '';
    has Str  $.text = '';
    has      %.attrs;
    has Bool $.self-closing = False;
}

{
    my @t;
    for <a b c> -> $text { @t.push: Token.new(kind => 'text', :$text) }
    is @t.map(*.text).join(','), 'a,b,c', 'each construction keeps its own named values';
    is @t.map(*.name).join(','), ',,', 'an omitted attribute takes its default every time';
}

{
    my %a = class => 'x';
    my @t;
    for ^3 { @t.push: Token.new(kind => 'start', name => 'div', attrs => %a, self-closing => False) }
    is @t[2].attrs<class>, 'x', 'a hash attribute is filled on a later call';
    %a<class> = 'y';
    is @t[0].attrs<class>, 'x', '... with its own copy of the hash';
}

{
    my @t;
    my @names = <p q r>;
    for @names.map({ "<$_>" }) -> $text { @t.push: Token.new(kind => 'text', :$text) }
    is @t.map(*.text).join, '<p><q><r>', 'a value read from a .map Seq';
}

{
    my @t;
    my %args = kind => 'end', name => 'li';
    for ^3 { @t.push: Token.new(|%args) }
    is @t.map(*.name).join(','), 'li,li,li', 'a |-spread argument list';
}

{
    my @t;
    for ^3 -> $i { @t.push: Token.new(kind => 'text', text => (1, 2, 3).map(* * $i).join(' ')) }
    is @t[2].text, '2 4 6', 'a value built by a call';
}

dies-ok { Token.new(name => 'x') }, 'a missing required attribute still dies';
for ^2 { Token.new(kind => 'x') }
dies-ok { Token.new(name => 'x') }, '... after the class is in the lane';

{
    class C { has $.v; method new(:$v!) { self.bless(v => $v * 10) } }
    my @c;
    for 1..3 -> $v { @c.push: C.new(:$v) }
    is @c.map(*.v).join(','), '10,20,30', 'a class with its own new is never replayed';
}

{
    class W { has $.x = 1 }
    my @w;
    for ^3 { @w.push: W.new(x => 2) }
    W.^add_method('extra', method { 42 });
    @w.push: W.new(x => 5);
    is @w[3].x, 5, 'a class changed by the MOP still constructs';
    is @w[3].extra, 42, '... with its new method';
}

{
    class J { has $.v }
    my @j;
    for ^3 { @j.push: J.new(v => 1 | 2) }
    ok @j[2].v ~~ Junction, 'a Junction value is stored, not autothreaded';
}

{
    my class Typed { has Int $.n }
    for ^2 { Typed.new(n => 1) }
    throws-like { Typed.new(n => 'x') }, X::TypeCheck, 'a type check still fails on a later call';
}
