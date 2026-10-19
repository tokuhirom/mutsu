use Test;

my Int @source = 1, 2;
sub copied(Int @values is copy) { @values.push('s'); @values }
is-deeply copied(@source), [1, 2, 's'], 'typed argument binds before its untyped copy is made';
is-deeply @source.List, (1, 2), 'copy does not mutate the source';
throws-like { copied([1, 2]) }, X::TypeCheck::Binding::Parameter,
    'an untyped argument is still rejected';
my Str @wrong = 'x';
my &dynamic = &copied;
throws-like { dynamic(@wrong) }, X::TypeCheck::Binding::Parameter,
    'a differently typed argument is rejected';

sub named(Int :@values! is copy) { @values.push('s'); @values }
is-deeply named(values => @source), [1, 2, 's'], 'named copy stays untyped';
class C {
    method copied(Int @values is copy) { @values.push('s'); @values }
}
is-deeply C.copied(@source), [1, 2, 's'], 'method copies follow the same order';
sub hash-copy(Int %values is copy) { %values<x> = 's'; %values<x> }
my Int %source = x => 1;
is hash-copy(%source), 's', 'typed hash copy has no element constraint';
is %source<x>, 1, 'hash source is unchanged';
done-testing;
