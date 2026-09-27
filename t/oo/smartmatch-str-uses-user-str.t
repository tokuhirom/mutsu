use Test;

# `$obj ~~ "text"` is `Str.ACCEPTS($obj)`, which compares against the
# object's `.Stringy` (by default its `.Str`), so a class's own (or inherited)
# stringification decides the match. From Tinky, whose
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
ok Both.new ~~ 'x', '.Stringy wins over .Str';
nok Both.new ~~ 'y', 'so .Str alone does not match';

class Boom { method Str() { die 'no Str here' } }
my $r;
lives-ok { $r = Boom.new ~~ 'x' }, 'a dying .Str does not escape the smartmatch';
nok $r, 'and is a non-match';
