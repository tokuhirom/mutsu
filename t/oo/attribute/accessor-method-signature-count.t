use Test;

# An auto-generated attribute accessor takes only its invocant: its signature
# is `(Class:D $:: *%_)`, so `.count`/`.arity` are 1. mutsu reported a raw
# capture signature with `.count` Inf, and Template::Jinja2's host-object
# attribute lookup (`$obj.can($attr)[0].count <= 1`) refused every accessor.

plan 5;

class P { has Str $.name = 'ada'; has $.rw is rw = 3 }
my $m = P.can('name')[0];
is $m.count, 1, '.count';
is $m.arity, 1, '.arity';
is $m.signature.raku, ':(P:D $:: *%_)', 'signature';
is $m(P.new), 'ada', 'still invocable';
is P.^lookup('rw').count, 1, 'an rw accessor too';
