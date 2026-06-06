use Test;

# Regression: a standalone trailing declarator comment (#= on its own line)
# must produce the correct WHEREFORE type in $=pod for methods, submethods,
# protos and anon subs with return types -- not a generic "Sub".
# Previously the callable type / proto / return-type metadata was dropped when
# the #= comment lived on a line after the declaration.

plan 7;

class Sheep {
    method roar { 'roar!' }
    #={not too scary}
}
is $=pod[0].WHEREFORE.^name, 'Method', 'method trailing #= -> Method WHEREFORE';

class C {
    submethod BUILD
    #={Bob}
    { }
}
is $=pod[1].WHEREFORE.^name, 'Submethod', 'submethod trailing #= -> Submethod WHEREFORE';

proto sub foo() { }
#={solo}
is $=pod[2].WHEREFORE.^name, 'Routine', 'proto sub trailing #= -> Routine WHEREFORE';

my $anon-sub = anon Str sub {};
#={Anonymous}
is $=pod[3].WHEREFORE.^name, 'Sub+{Callable[Str]}',
    'anon Str sub trailing #= -> Sub+{Callable[Str]} WHEREFORE';

# The pod entry and the declarand's .WHY are the same logical declarator.
is ~$=pod[0], 'not too scary', 'method trailing contents';
is $=pod[0].WHEREFORE.^name, Sheep.^find_method('roar').^name,
    '$=pod WHEREFORE name matches the method';

# A plain sub with no comment has no WHY.
sub routine {}
is &routine.WHY.defined, False, 'undocumented sub has no WHY';
