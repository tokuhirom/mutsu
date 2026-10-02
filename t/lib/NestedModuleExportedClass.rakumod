module NestedModuleExportedClass {
    my class NestedModExp is export { method hi { "hi from module" } }
}

sub make-it { my class RoutineExp is export { method hi { "hi from sub" } } }
