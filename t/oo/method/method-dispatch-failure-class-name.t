use Test;

# The invocant type in a multi-dispatch failure message is the class's declared
# name, also for a lexical class (`my class`), which is registered under a
# serial-suffixed name internally (#16621). Expectations measured against
# rakudo 2026.09.

plan 6;

my class M { multi method f(Int) { "i" } }
try M.new.f("a");
is $!.^name, 'X::Multi::NoMatch', 'a failing multi method call';
is $!.message.lines[0], 'Cannot resolve caller f(M:D: Str:D); none of these signatures matches:',
    'names the declared class';

try M.f("a");
is $!.message.lines[0], 'Cannot resolve caller f(M:U: Str:D); none of these signatures matches:',
    'a type-object invocant';

my class N is M { multi method g(Int) { "g" } }
try N.new.g("x");
is $!.message.lines[0], 'Cannot resolve caller g(N:D: Str:D); none of these signatures matches:',
    'a subclass';

{
    my class Inner { multi method h(Int) { "h" } }
    try Inner.new.h("x");
    is $!.message.lines[0], 'Cannot resolve caller h(Inner:D: Str:D); none of these signatures matches:',
        'a class declared inside a block';
}

class Top { multi method k(Int) { "k" } }
try Top.new.k("x");
is $!.message.lines[0], 'Cannot resolve caller k(Top:D: Str:D); none of these signatures matches:',
    'a package class is unchanged';
