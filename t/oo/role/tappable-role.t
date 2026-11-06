use Test;

# The core `Tappable` role (the base of Supply's tap protocol) can be composed
# by user classes. Found in the EEL distribution, whose `MatcherSupply` is a
# `class ... does Tappable`.

plan 10;

class A does Tappable {
    method tap(&emit, &done, &quit, &tap) { 42 }
    method live { True }
    method sane { True }
}
my $a = A.new;
ok $a ~~ Tappable, 'an instance does Tappable';
is A.^roles.map(*.^name).List, ('Tappable',), 'the role is composed';
ok $a.serial === $a, 'serial is inherited and returns the invocant';
is $a.tap(-> $ { }, -> { }, -> $ { }, -> $ { }), 42, 'the class tap is used';
is Tappable.^name, 'Tappable', 'the role has its name';
is Tappable.^methods(:local).map(*.name).sort.List, <live sane serial tap>,
    'the role declares tap, live, sane and serial';

class B does Tappable {
    method tap(|) { 1 }
    method live { False }
    method sane { True }
    method serial { "mine" }
}
is B.new.serial, 'mine', 'a class may override serial';
nok B.new.live, 'live comes from the class';

# The stubs are required methods, as in rakudo.
throws-like 'class C does Tappable { method tap(|) { 1 }; method sane { True } }',
    X::Comp::AdHoc, 'a class without live fails to compose';
throws-like 'class D does Tappable { method live { True }; method sane { True } }',
    X::Comp::AdHoc, 'a class without tap fails to compose';
