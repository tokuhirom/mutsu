use Test;
use lib $*PROGRAM.parent(2).add('lib');
use ModuleClassShadow::Lib;

plan 2;

# Inside a module, its own `class Test` is what the bare name means, even in
# a program that also loaded the `Test` module (TAP's `TAP::Test`).
is ModuleClassShadow::Lib::type-name(), 'ModuleClassShadow::Lib::Test', 'bare name resolves to the package class';
is ModuleClassShadow::Lib::make().tests, 1, 'grep by the package class';
