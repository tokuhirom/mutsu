use v6;
unit role RoleUnrelatedModuleUser;
use RoleUnrelatedModuleShapes;

# `Event::Test` shares no textual prefix with the module that supplies it
# (`RoleUnrelatedModuleShapes`) -- its own internal package is named
# `Outer`, unrelated to both the file name and the module name (#8023).
method describe(Event::Test:U \x) { "qualified-ok" }

# Exercised entirely from inside this compunit, so the test script itself
# never needs to import `RoleUnrelatedModuleShapes` (which would load it
# before this role's own body does, defeating the not-yet-loaded-module
# condition #8023 is actually about).
method self-test() {
    self.describe(Event::Test);
}
