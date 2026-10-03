use Test;

# An attribute typed with a lexical class (`my class P`) reports that type
# object from `.type` -- the same object, with its own attributes -- even when
# introspected after the declaring scope has exited. From JSON::Unmarshal's
# `$attr.type.^attributes` on a test's `my class TestClassPos does Positional`.

plan 6;

my class P does Positional { has Str $.string }
my class T { has P $.pos }

my $type := T.^attributes[0].type;
ok $type === P, '.type is the lexical class itself';
is $type.^attributes.map(*.name).List, ('$!string',), 'with its attributes';
is $type.^name, 'P', 'and its short name';

sub mk {
    my class Inner { has $.v }
    my class Outer { has Inner $.in }
    Outer
}
my $outer = mk();
is $outer.^attributes[0].type.^attributes.elems, 1, 'after the declaring sub returned';

throws-like { T.new(pos => 5) }, X::TypeCheck::Assignment, message => /'expected P'/,
    'the type check still names the short type';
is T.new(pos => P.new(string => 'x')).pos.string, 'x', 'construction is unaffected';
