use Test;

# #16718: `RakuAST::Name.qualified-with(Name)` -- a one-argument row on a Name
# instance. The qualifier's parts come first; a name that starts with `GLOBAL`
# is already absolute and answers its own parts without that first one.

plan 8;

sub id($n) { RakuAST::Name.from-identifier($n) }
sub parts(*@p) { RakuAST::Name.from-identifier-parts(|@p) }

is id('x').qualified-with(id('P')).canonicalize, 'P::x', 'a qualifier is prefixed';
is parts('A', 'B').qualified-with(parts('P', 'Q')).canonicalize, 'P::Q::A::B',
    'multi-part names on both sides';
is parts('GLOBAL', 'Foo').qualified-with(id('P')).canonicalize, 'Foo',
    'a leading GLOBAL is dropped and the qualifier ignored';
is id('x').qualified-with(parts('GLOBAL', 'Q')).canonicalize, 'GLOBAL::Q::x',
    'a GLOBAL qualifier is kept';
isa-ok id('x').qualified-with(id('P')), RakuAST::Name, 'the answer is a Name';

my $n = id('x');
$n.qualified-with(id('P'));
is $n.canonicalize, 'x', 'the receiver is not changed';

throws-like { id('x').qualified-with(5) }, X::TypeCheck::Binding::Parameter,
    'a non-node target is rejected';
throws-like { id('x').qualified-with(RakuAST::IntLiteral.new(1)) },
    X::TypeCheck::Binding::Parameter, 'so is a node that is not a Name';
