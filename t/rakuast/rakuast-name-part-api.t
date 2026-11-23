use Test;
use experimental :rakuast;

plan 13;

# `name`, `is-empty` and `is-pseudo-package` on the `RakuAST::Name::Part::*`
# classes, as raku-doc/doc/Type/RakuAST/Name/Part/*.rakudoc document them and
# rakudo 2026.11 answers (#18977).

my $e = RakuAST::Name::Part::EmptyEdge.new;
is-deeply $e.name, '', 'EmptyEdge.name';
is-deeply $e.is-empty, True, 'EmptyEdge.is-empty';
is-deeply $e.is-pseudo-package, False, 'EmptyEdge.is-pseudo-package';

is-deeply RakuAST::Name::Part::Simple.new('').is-empty, True, 'an empty simple part is empty';
is-deeply RakuAST::Name::Part::Simple.new('a').is-empty, False, 'a named one is not';

my $x = RakuAST::Name::Part::Expression.new(RakuAST::StrLiteral.new('foo'));
is-deeply $x.is-empty, False, 'Expression.is-empty';
is $x.name, 'foo', 'Expression.name is the value it holds';
is-deeply RakuAST::Name::Part::Expression.new(RakuAST::IntLiteral.new(3)).name, 3,
    '... of any literal';

is-deeply <foo CALLER MY OUTER UNIT>.map({ RakuAST::Name::Part::Simple.new($_).is-pseudo-package }).List,
    (False, True, True, True, True), 'pseudo-package parts';
is-deeply RakuAST::Name::Part::Simple.new('GLOBAL').is-pseudo-package, False, 'GLOBAL is not one';
is-deeply RakuAST::Name::Part::Simple.new('PROCESS').is-pseudo-package, False, 'nor PROCESS';
is-deeply RakuAST::Name::Part::Simple.new('CORE').is-pseudo-package, True, 'CORE is';
is RakuAST::Name::Part::EmptyEdge.^methods(:local).map(*.name).grep(!*.starts-with('IMPL')).sort.join(','),
    'is-empty,name,new', "EmptyEdge's own methods";
