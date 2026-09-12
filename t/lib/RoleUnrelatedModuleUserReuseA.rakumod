use v6;
unit role RoleUnrelatedModuleUserReuseA;
use RoleUnrelatedModuleShapes;

method first(Event::Test:U \x) { 'a' }

method self-test() {
    self.first(Event::Test:U)
}
