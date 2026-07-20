use v6;
use lib 't/lib';
use Test;

plan 3;

# A role whose body `use`s a qualified class and then references it (with a `:D`
# smiley) in a method parameter / return type must load. The role's param-type
# validation used to run before the body's `use` had loaded the module, so the
# imported qualified type looked unresolvable and threw
# "Invalid typename '...:D' in parameter declaration." (regression: Testo dist).

use RoleParamImport::Out;
use RoleParamImport::Result;

pass 'role with a body-imported qualified :D param type loads';

class Consumer does RoleParamImport::Out {}

my $r = RoleParamImport::Result.new(value => 42);
my $out = Consumer.new.put($r);
isa-ok $out, RoleParamImport::Result, 'put() returns the imported type';
is $out.describe, 'result=42', 'the returned object round-trips';
