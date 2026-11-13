use Test;

# `$obj ~~ "text"` is `Str.ACCEPTS($obj)`, which compares against the
# object's `.Str`, so a class's own (or inherited) stringification decides
# the match; a `.Str` that dies, or answers a non-string object, dies it. From Tinky, whose
# `Workflow.state($name)` finds a state with `.first({ $_ ~~ $name })`.

plan 10;

class S {
    has $.name;
    method Str() { $!name }
}
class T is S { }
class NoStr { has $.name }

my $s = S.new(name => 'new');
ok $s ~~ 'new', 'Instance ~~ Str uses the user .Str';
nok $s ~~ 'old', 'and fails when .Str differs';
ok 'new'.ACCEPTS($s), 'Str.ACCEPTS is the same check';
ok T.new(name => 'x') ~~ 'x', 'an inherited .Str counts';
is (S.new(name => 'a'), $s).first({ $_ ~~ 'new' }).name, 'new', 'works as a .first matcher';
nok NoStr.new(name => 'x') ~~ 'x', 'a class without .Str does not match its attribute';

class Both { method Stringy() { 'x' }; method Str() { 'y' } }
nok Both.new ~~ 'x', '.Stringy is not consulted';
ok Both.new ~~ 'y', '.Str is';

class Boom { method Str() { die 'no Str here' } }
dies-ok { Boom.new ~~ 'x' }, 'a dying .Str escapes the smartmatch';
class Selfish { method Str() { self } }
throws-like { Selfish.new ~~ 'x' }, Exception,
    message => /'cannot unbox to a native string'/,
    'a .Str answering a non-string object dies';
