use Test;

plan 8;

# rakudo's `variable` token lets a `.`-twigil variable take a colon arglist
# (`$.meth: args`), and `has` reuses it: `has Int $.gid:` followed by a newline
# parses the NEXT declaration as the first one's arglist. The arglist's value
# is discarded, but a declaration inside it still declares. (#9321)

class A {
    has Int $.gid:
    has Str @.m;
}

{
    my $a = A.new(gid => 3, m => <x y>);
    is $a.gid, 3, 'the attribute before the colon exists';
    is-deeply $a.m, Array[Str].new(<x y>), 'the declaration in its arglist exists too';
    is A.^attributes.map(*.name).join(','), '@!m,$!gid',
        'the nested declaration is installed first, as in rakudo';
}

class B { has $.x: 42; }
is B.new.x, Any, 'a plain arglist value is not a default';

class C { has $.x: has $.y: has $.z }
is C.new(z => 1).z, 1, 'colon arglists chain';

class D { has Int $.gid: has Str @.m = <a>; }
is-deeply D.new.m, Array[Str].new('a'), 'a nested declaration keeps its own default';

class E { has @.list: 1; }
is-deeply E.new.list, [], 'an @-sigil attribute takes a colon arglist too';

throws-like 'class F { has $!x: 3; }', X::Syntax::Confused,
    'a private attribute has no colon arglist';
