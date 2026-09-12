use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# Rakudo builds NO node for a slurpy marker: the
# `RakuAST::Parameter::Slurpy::*` type object itself is what `$!slurpy` holds.
# mutsu normalized the other way -- type object in, empty node out -- so the two
# rendered identically inside the parent's gist but the field itself read as
# defined, gisted as the full class name, and compared unequal to the very class
# it named. On rakudo the test for "is this parameter slurpy?" is which CLASS of
# type object the field holds, never definedness, so mutsu's `True` was the
# opposite of useful (#8157).
#
# Every expectation below was measured against rakudo.

plan 19;

my $flat = Q[sub f(*@c) { }].AST.statements[0].expression.signature.parameters[0];
my $unflat = Q[sub h(**@c) { }].AST.statements[0].expression.signature.parameters[0];
my $plain = Q[sub g($x) { }].AST.statements[0].expression.signature.parameters[0];

# The field holds a type object, so it is undefined.
nok $flat.slurpy.defined, 'a slurpy marker is a type object, so it is not defined';
nok $unflat.slurpy.defined, 'the same for **@c';
nok $plain.slurpy.defined, 'and for a parameter with no marker at all';

# Its name identifies which marker it is.
is $flat.slurpy.^name, 'RakuAST::Parameter::Slurpy::Flattened',
    '*@c carries the Flattened marker';
is $unflat.slurpy.^name, 'RakuAST::Parameter::Slurpy::Unflattened',
    '**@c carries the Unflattened marker';
is $plain.slurpy.^name, 'RakuAST::Parameter::Slurpy',
    'a non-slurpy parameter carries the base class';

# `.gist` of a type object on its own is the short `(Name)` form.
is $flat.slurpy.gist, '(Flattened)', '.gist of the marker alone is (Flattened)';
is $unflat.slurpy.gist, '(Unflattened)', 'and (Unflattened)';
is $plain.slurpy.gist, '(Slurpy)', 'and (Slurpy) for the base class';

# `.raku` is the full class name, unlike `.gist`.
is $flat.slurpy.raku, 'RakuAST::Parameter::Slurpy::Flattened',
    '.raku of the marker is its full class name';

# Identity against the class is how rakudo asks which marker this is.
ok $flat.slurpy === RakuAST::Parameter::Slurpy::Flattened,
    'the marker is identical to the class it names';
ok $flat.slurpy =:= RakuAST::Parameter::Slurpy::Flattened,
    '=:= agrees with ===';
ok $unflat.slurpy === RakuAST::Parameter::Slurpy::Unflattened,
    'and for the Unflattened marker';
nok $flat.slurpy === RakuAST::Parameter::Slurpy::Unflattened,
    'the two markers are not identical to each other';

# The documented idiom: compare against the BASE class, not `.defined`.
ok $flat.slurpy !=== RakuAST::Parameter::Slurpy, '*@c is slurpy by the base-class test';
nok $plain.slurpy !=== RakuAST::Parameter::Slurpy, '$x is not';

# Type-check membership is unchanged.
ok $flat.slurpy ~~ RakuAST::Parameter::Slurpy, 'the marker does the base class';

# The parent's gist still shows the marker as its bare class name -- a type
# object renders differently inside a node's gist than it does alone.
ok Q[sub f(*@c) { }].AST.gist.contains('slurpy => RakuAST::Parameter::Slurpy::Flattened'),
    "the parent's gist renders the marker as its bare class name";

# A hand-built parameter keeps the type object it was given, and still lowers.
{
    my $built = RakuAST::Parameter.new(
        target => RakuAST::ParameterTarget::Var.new(name => '@values'),
        slurpy => RakuAST::Parameter::Slurpy::Flattened,
    );
    ok $built.slurpy === RakuAST::Parameter::Slurpy::Flattened,
        '.new keeps the type object it was handed';
}

done-testing;
