use Test;

# A method called on a plain role's type object runs on the role's one
# persistent pun class, as in rakudo (#17408): the pun is composed once (the
# role body runs once), it is a class (`self.HOW`), and it leaves the role
# itself -- its composition into classes, `.new`, introspection -- unchanged.

plan 20;

my $runs = 0;
role R {
    $runs++;
    my $x = 7;
    method gx { $x }
    method how { self.HOW.^name }
    method cls { $?CLASS.^name }
    method rl { $?ROLE.^name }
    method me { self }
    method !priv { 'p' }
    method callp { self!priv }
    method kv { my class KV { method v { 42 } }; KV.v }
}

is R.gx, 7, 'a role body lexical from a method on the type object';
is R.gx, 7, '... on a second call';
is $runs, 1, 'the role body ran once';
is R.how, 'Perl6::Metamodel::ClassHOW', 'the method runs on a class (the pun)';
is R.cls, 'R', '$?CLASS is the pun, named after the role';
is R.rl, 'R', '$?ROLE is the role';
is R.me.^name, 'R', 'self reports the role name';
nok R.me === R, 'self is the pun, not the role type object';
ok R.me ~~ R, 'the pun does the role';
is R.callp, 'p', 'a private method of the role';
is R.kv, 42, 'a class declared in a role method';

is R.^name, 'R', 'the role keeps its name';
is R.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW', 'the role is still a role';

class C does R { }
is C.new.gx, 7, 'a class composing the role afterwards';
is C.^roles.map(*.^name).join(','), 'R', '... does exactly the role';
is R.new.gx, 7, '.new on the role still puns';
ok R.new ~~ R, '... to something that does the role';

role S does R { method s { self.gx + 1 } }
is S.s, 8, 'a role that does a role';

role A { has $.a = 5; method sa { self.a } }
dies-ok { A.sa }, 'an attribute read on the pun type object dies';

is (await start { R.gx }), 7, 'from another thread';
