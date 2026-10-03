use Test;

# `.?method` on a routine that has no such method is Nil. mutsu used to
# answer the Sub its last-resort method composition builds for a callable
# (`<composed-method:...>`), which made upstream NativeCall's
# `my str $conv = self.?native_call_convention || ''` fail its type check.

plan 6;

my $plain := sub foo() { 42 };
ok $plain.?no-such-method =:= Nil, '.? of a missing method on a Sub is Nil';
is $plain.?name, 'foo', '.? of an existing method still calls it';

my role Conv[$name] { method native_call_convention() { $name } }
my role Probe { method conv() { self.?native_call_convention || '' } }

my $r := sub bar() { };
$r does Probe;
is $r.conv, '', '.? on self inside a role mixed into a routine';
$r does Conv['stdcall'];
is $r.conv, 'stdcall', 'and it calls the method once a role supplies it';

ok { $_ }.?nope =:= Nil, '.? of a missing method on a Block is Nil';
ok (* + 1).?nope =:= Nil, '.? of a missing method on a WhateverCode is Nil';
