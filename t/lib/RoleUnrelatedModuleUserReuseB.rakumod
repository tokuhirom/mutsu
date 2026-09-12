use v6;
unit role RoleUnrelatedModuleUserReuseB;
use RoleUnrelatedModuleShapes;

method second(Event::Test \x) { 'b' }

method self-test() {
    self.second(Event::Test)
}
