use v6;
unit role RoleUnrelatedModuleUserNoSmiley;
use RoleUnrelatedModuleShapes;

# Same as RoleUnrelatedModuleUser.rakumod but with no definiteness smiley on
# the parameter -- kept in a separate role so this doesn't also exercise the
# unrelated pre-existing bug tracked as #8084 (two methods in the same role
# constrained by the identical body-imported qualified type name).
method describe-no-smiley(Event::Test \x) { "qualified-no-smiley-ok" }

method self-test() {
    self.describe-no-smiley(Event::Test);
}
