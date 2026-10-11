use Test;

# From Map::Agnostic's t/01-basic.rakutest: a stringifier called on a role type
# object is answered by Mu, not by punning the role. Punning a role that still
# has a `method m { ... }` stub would die with "must be implemented".

plan 5;

role Base {
    multi method Str(::?CLASS:U:) { "" }
    multi method Str(::?CLASS:D:) { "d" }
}
role R does Base {
    method INIT-KEY(\key) { ... }
    method other() { "other" }
}

is R.raku, 'R', '.raku on a role with a stub does not pun';
is (quietly R.Str), '', '.Str on a role with a stub does not pun';
is R.gist, '(R)', '.gist on a role with a stub does not pun';

role S { method foo() { "foo" } }
is S.foo, 'foo', 'a non-Mu method still puns the role';
dies-ok { R.other }, 'a non-Mu method on a role with a stub still refuses to pun';

done-testing;
