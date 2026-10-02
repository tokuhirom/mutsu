use v6;
use Test;
use lib 't/lib';

# From Tinky::Declare: a package that `use`s a module and then declares its
# own class whose short name collides with one of the module's classes must
# resolve the bare name to its own class.

plan 3;

module Foo::Bar {
    use ShadowImportLib;
    class Workflow is ShadowImportLib::Workflow {
        method hi { "sub" }
    }
    our sub mk($n) { Workflow.new(name => $n) }
    role Role1 { }
}

my $w = Foo::Bar::mk("y");
is $w.^name, 'Foo::Bar::Workflow', 'bare name resolves to the local class';
is $w.hi, 'sub', 'local class methods are used';
is $w.name, 'y', 'inherited attribute still works';
