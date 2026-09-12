use v6;
use lib 't/lib';
use Test;

plan 1;

# #8023: a `::`-qualified type imported from a module `use`d inside a role
# body was rejected in a role method's parameter ("Invalid typename '...' in
# parameter declaration."), even though the identical parameter type is
# accepted in a class/module/package. The role's own accept-heuristic for a
# type supplied by a not-yet-loaded body `use` matched the constraint's name
# against the MODULE's name -- sound only when the type's qualified spelling
# happens to share a prefix with the module (`RoleParamImport::Result` from
# `use RoleParamImport::Result;`), but `Event::Test` shares nothing with the
# module that actually supplies it here (`RoleUnrelatedModuleShapes`, whose
# own internal package is named `Outer`). Only `use`ing the role itself here
# (not `RoleUnrelatedModuleShapes` too) keeps that module genuinely
# not-yet-loaded at the role's own registration time -- loading it
# independently first would resolve the type a different way and miss the
# bug.
#
# Kept to a single role/method/process: a second body-deferred resolution of
# the identical qualified name anywhere else in the same process (a second
# method in this role, or a second role importing the same module) hits a
# separate, pre-existing bug tracked as #8084. The `:U`-smiley sibling case
# is covered by role-param-qualified-type-unrelated-module-no-smiley.t, in
# its own process for the same reason.

use RoleUnrelatedModuleUser;

class Consumer does RoleUnrelatedModuleUser {}

is Consumer.new.self-test(), "qualified-ok",
    'a `::`-qualified imported type with a :U smiley is accepted in a role method parameter and resolves for real';
