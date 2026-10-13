use Test;
use lib 't/lib';
use TypedStaticPerModule::ListKeyed;
use TypedStaticPerModule::HashKeyed;

# Two modules' class-body statics of the same name keep their own declared
# key and element types (#12903): the second module's `my %seen{Hash}` used
# to overwrite the name-keyed metadata the first class's methods checked
# their stores against.

plan 7;

is TypedStaticPerModule::ListKeyed.put((1, 2)), 1, 'List-keyed store in the first module';
is TypedStaticPerModule::HashKeyed.put({ a => 1 }), 1, 'Hash-keyed store in the second module';
is TypedStaticPerModule::ListKeyed.put((3, 4)), 2, 'the first module keeps its own hash';
is TypedStaticPerModule::ListKeyed.count(5), 5, 'Int element store in the first module';
is TypedStaticPerModule::HashKeyed.count('s'), 's', 'Str element store in the second module';
throws-like { TypedStaticPerModule::ListKeyed.count('x') }, X::TypeCheck,
    'the first module still rejects a Str element';
throws-like { TypedStaticPerModule::ListKeyed.put({ b => 2 }) }, X::TypeCheck,
    'the first module still rejects a Hash key';
