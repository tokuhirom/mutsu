use v6;
unit role RoleUnrelatedModuleUserReuse;
use RoleUnrelatedModuleShapes;

method first(Event::Test:U \x) { 'm1' }
method second(Event::Test \x) { 'm2' }

method self-test() {
    self.first(Event::Test) ~ self.second(Event::Test)
}
