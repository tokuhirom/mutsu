use Test;

# A `$!attr = v` / `$.attr = v` write from inside a method must honour the
# attribute's declared type, exactly like the `self.attr = v` accessor form.
# Until 2026-07-25 the twigil forms were compiled as ordinary name assignments
# and never consulted the class registry, so a typed attribute was unenforced
# from inside its own class.

plan 22;

class Priv { has Int $.n is rw; method set($v) { $!n = $v } }
my $p = Priv.new(n => 1);
lives-ok { $p.set(2) }, 'private twigil accepts a conforming value';
is $p.n, 2, 'and stores it';
throws-like { $p.set("nope") }, X::TypeCheck::Assignment,
    'private twigil rejects a wrong type';
is $p.n, 2, 'and the rejected value is not stored';

class Pub { has Int $.n is rw; method set($v) { $.n = $v } }
my $q = Pub.new(n => 1);
lives-ok { $q.set(3) }, 'public twigil accepts a conforming value';
is $q.n, 3, 'and stores it';
throws-like { $q.set("nope") }, X::TypeCheck::Assignment,
    'public twigil rejects a wrong type';
is $q.n, 3, 'and the rejected value is not stored';

class Acc { has Int $.n is rw; method set($v) { self.n = $v } }
my $a = Acc.new(n => 1);
throws-like { $a.set("nope") }, X::TypeCheck::Assignment,
    'the accessor form still rejects a wrong type';

# A subset behaves like any other declared type.
subset RequestMethod of Str where any(<GET POST HEAD>);
class Req {
    has RequestMethod $.method is rw = 'GET';
    method set-method($m) { $!method = $m }
}
my $r = Req.new;
lives-ok { $r.set-method('POST') }, 'a subset accepts a member';
is $r.method, 'POST', 'and stores it';
throws-like { $r.set-method('TEST') }, X::TypeCheck::Assignment,
    'a subset rejects a non-member';
is $r.method, 'POST', 'and the rejected value is not stored';

# Nil resets a typed attribute to its own type object rather than dying.
class Reset { has Int $.n is rw = 5; method clear { $!n = Nil } }
my $z = Reset.new;
lives-ok { $z.clear }, 'Nil assignment lives';
ok $z.n === Int, 'and resets the attribute to its type object';

# An untyped attribute stays unconstrained.
class Untyped { has $.a is rw; method set($v) { $!a = $v } }
my $u = Untyped.new;
lives-ok { $u.set('str') }, 'an untyped attribute takes anything';
is $u.a, 'str', 'and stores it';

# `@`/`%` attributes constrain their ELEMENTS, not the container, so a whole
# container write from inside a method must not be checked against the element
# type.
class Cont {
    has Int @.list;
    has Int %.map;
    method go { @!list.push(3); %!map<k> = 4 }
}
my $c = Cont.new;
lives-ok { $c.go }, 'typed container attributes are unaffected';
is $c.list, [3], 'array attribute keeps its elements';
is $c.map<k>, 4, 'hash attribute keeps its values';

# The check also covers a write from `submethod BUILD`.
class Built { has Int $.v; submethod BUILD(:$v) { $!v = $v } }
is Built.new(v => 7).v, 7, 'BUILD accepts a conforming value';
throws-like { Built.new(v => 'x') }, X::TypeCheck::Assignment,
    'BUILD rejects a wrong type';
