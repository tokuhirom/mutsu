unit module ModuleClassShadow::Lib;
class Test { has $.ok }
class Sub-Test is Test {
    has @.entries;
    method tests() { @!entries.grep(Test).elems }
}
our sub make() { Sub-Test.new(ok => True, entries => [Test.new(ok => True), 1]) }
our sub type-name() { Test.^name }
