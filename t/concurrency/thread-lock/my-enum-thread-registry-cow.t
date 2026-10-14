use Test;

# A `my enum` declared below the unit's top level is registered once, when the
# unit loads, so a run of its body on a thread writes no table the thread
# shares with its parent (#13010; the copy counts are pinned by
# tests/my_enum_thread_registry_cow.rs). The registration ahead of time must
# not change what the enum means: it is still lexical to its block, and its
# values are the same on every thread. Every expectation was checked against
# rakudo.

plan 9;

class Zed { }
sub f() { my enum E <Zed Yon>; Zed }
nok (try ::('E')).defined, 'a routine\'s my enum is not visible by name before the routine runs';
is Zed.^name, 'Zed', 'its variant does not shadow an outer type before the routine runs';
is f().raku, 'E::Zed', 'the routine sees its own variant';
is Zed.^name, 'Zed', 'nor after the routine ran';
is f().^name, 'E', 'the variant belongs to the enum';

class C { method m { my enum CE <cx cy>; cy } }
is C.new.m.raku, 'CE::cy', 'a my enum in a method';
nok (try ::('CE')).defined, 'is not visible by name outside the method';

sub g() { my enum E2 <a b>; b.value }
is-deeply (await (start { g() }), (start { g() })), (1, 1), 'the enum has the same values on every thread';

class P { method parse() { supply { my enum PE <RequestLine Header>; emit Header } } }
my @p = (^4).map: { start { my $r; P.new.parse.tap({ $r = $_ }); $r.raku } };
is-deeply (await @p).unique.List, ('PE::Header',), 'a supply body\'s my enum, run on threads';
