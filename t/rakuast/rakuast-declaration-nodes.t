use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# Declarations the parser keeps as written nodes: a parenthesized `has`
# group, an attribute colon argument list and a `.=` declaration. Each is one
# node for `.AST`, and lowers (via EVAL) to the same program as the source.

plan 12;

sub declaration-of(Str $source) { $source.AST.statements[0].expression }
sub attribute-of(Str $source) { ('class C { ' ~ $source ~ ' }').AST.statements[0].expression.body.body.statement-list.statements[0].expression }

is attribute-of('has Int ($.a, $.b);').^name, 'RakuAST::VarDeclaration::Signature',
    'a grouped has is one signature declaration';
is declaration-of('my Int $s .= new(3);').^name, 'RakuAST::VarDeclaration::Simple',
    'a .= declaration is a simple declaration';
is declaration-of('my Int $s .= new(3);').initializer.^name,
    'RakuAST::Initializer::CallAssign', 'its initializer is the call-assign';

my $group = 'class G { has Int ($.p, $.q); }; G.new(:p(2), :q(4)).q + G.new(:p(2)).p'.AST.EVAL;
is $group, 6, 'a grouped has installs both attributes';

my $colon = 'class D { has Int $.gid: has Str @.m; }; D.new.^attributes.elems'.AST.EVAL;
is $colon, 2, 'the declaration in a colon argument list still declares';

is 'my Int $s .= new(7); $s'.AST.EVAL, 7, 'a typed .= declaration runs';
is 'my $o .= new; $o.^name'.AST.EVAL, 'Any', 'an untyped .= declaration runs';
is 'my @a .= new(1, 2); @a.elems'.AST.EVAL, 2, 'an array .= declaration runs';
is 'my Int $s .= new(-3).abs; $s'.AST.EVAL, -3, 'a .= declaration with a chain keeps the call result';

is (my Int $e .= new(4)).^name, 'Int', 'a .= declaration in expression position';
is ('(my Int $i .= new(5)).^name'.AST.EVAL), 'Int', 'its RakuAST lowering runs';

my $round = 'class H { has Int ($.m, $.n); }; H.new(:m(1), :n(2)).n'.AST.EVAL;
is $round, 2, 'a grouped has round-trips through RakuAST';
