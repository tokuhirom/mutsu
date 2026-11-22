use experimental :rakuast;
use Test;

# `::Array` / `::Foo::Bar` / `::Int:D` (#18620): the type's `Name` starts
# with a `Name::Part::EmptyEdge`, as in Rakudo 2026.11, and spans the whole
# `::`-prefixed text. The leading `::` still looks the type up as before.

plan 9;

sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}

my $type = Q[say(::Array)].AST.statements[0].expression.args.args[0];
is $type.^name, 'RakuAST::Type::Simple', '::Array is a Type::Simple';
is $type.name.parts.map(*.^name).join(' '),
    'RakuAST::Name::Part::EmptyEdge RakuAST::Name::Part::Simple',
    'its name starts with the empty edge';
is $type.name.canonicalize, 'Array', 'its canonical name has no leading ::';

my $nested = Q[class Qux { class Quux {} }; say(::Qux::Quux)].AST.statements[1].expression.args.args[0];
is $nested.name.parts.elems, 3, '::Qux::Quux has the edge and two parts';

my $smiley = Q[say(::Int:D)].AST.statements[0].expression.args.args[0];
is $smiley.base-type.name.parts[0].^name, 'RakuAST::Name::Part::EmptyEdge',
    '::Int:D keeps the edge on its base type';

is placed(Q[say(1, ::Array, "x")]),
    'Call::Name@0-20 Name@0-3 ArgList@4-19 IntLiteral@4-5 Type::Simple@7-14 Name@7-14 QuotedString@16-19 StrLiteral@17-18',
    'the name spans ::Array';
is placed(Q[class Foo { class Bar {} }; say ::Foo::Bar]),
    'Class@0-26 Name@6-9 Block@10-26 Blockoid@10-26 Class@12-24 Name@18-21 Block@22-24 Blockoid@22-24 Call::Name::WithoutParentheses@28-42 Name@28-31 ArgList@32-42 Type::Simple@32-42 Name@32-42',
    'the name spans ::Foo::Bar';

is EVAL(Q[::Array.^name].AST), 'Array', 'EVAL of the tree looks the type up';
is ::Int:D.^name, 'Int:D', 'the leading :: still names the type at run time';
