use v6;
use lib 't/lib';
use Test;
use PlainExportHook;

plan 9;

# A unit that imports through a run-time `sub EXPORT` hook compiles the CORE
# term keywords `True`/`False`/`Nil` to a shadowable read (#9047), so an
# attribute default `= False` is a chunk rather than a literal. When no hook
# installed the name it is the constant it folds to: the constructor stores it
# as it stores a literal, without evaluating the chunk or collapsing the
# caller's scope for it (#16897). Checked against rakudo.
class T {
    has Str  $.kind is required;
    has Bool $.closed = False;
    has Bool $.open   = True;
    has      $.nothing = Nil;
    has Str  $.name = '';
}

my $t = T.new(kind => 'start');
ok $t.closed === False, '`= False` defaults to the Bool';
ok $t.open === True, '`= True` defaults to the Bool';
nok $t.nothing.defined, '`= Nil` resets the attribute to its default';
ok T.new(kind => 'x', :closed).closed === True, 'an argument still wins';

# The constructor lane, from a routine with lexicals of its own: the
# construction must neither need nor disturb the caller's scope.
sub build-many(Int $n) {
    my $marker = 'kept';
    my @made;
    for ^$n -> $i {
        @made.push: T.new(kind => 'k' ~ $i, name => $marker);
    }
    ($marker, |@made)
}
my ($marker, @made) = build-many(200);
is +@made, 200, 'a hot constructor loop builds every object';
is @made[199].kind, 'k199', 'with its own arguments';
ok @made.grep(*.closed === False) == 200, 'and the default on each';
is @made[0].name, 'kept', 'a caller lexical passed as an argument arrives';
is $marker, 'kept', 'and the caller lexical is untouched';
