use Test;

# WebService::TMDB's Test::Mock dependency chains ^compose's result into
# .CREATE when it builds a dynamic mock class. The parent assertion covers
# the cache invalidation that makes the mock satisfy its required role.
plan 4;

my $type = Metamodel::ClassHOW.new_type(name => 'ComposeReturnType');
my $composed = $type.HOW.compose($type);
is $composed, $type, '^compose returns the composed type object';
ok $composed.CREATE ~~ $type, 'the returned type object can allocate an instance';

class ComposeParent { }
my $parented = Metamodel::ClassHOW.new_type(name => 'ComposeParentedType');
$parented.HOW.add_parent($parented, ComposeParent);
$parented.HOW.compose($parented);
ok $parented.^mro.map(*.^name).grep('ComposeParent').elems == 1,
    'adding a parent invalidates the cached MRO';
ok $parented.CREATE ~~ ComposeParent,
    'instances of a dynamic type inherit the added parent';
