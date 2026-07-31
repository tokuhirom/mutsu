use Test;

plan 2;

# A closure created inside a METHOD body lexically belongs to that method's
# class: nested-class short names must resolve against it no matter which
# package the method was invoked from. `lexical_closure_package` preferred
# `current_package` (the CALLER's package — method dispatch does not re-point
# it), so a `start Inner.new(...)` inside a method invoked from module code
# captured the module's package and the suppressed nested name died with
# X::Undeclared::Symbols (Cro::CompositeConnector.connect ->
# TestConnector.connect -> `start Transform.new`).

module CallerPkg {
    our sub invoke($obj, %opts) { $obj.make(|%opts) }
}

class Outer {
    class Inner {
        has $.tag;
    }
    method make(*%opts) {
        start Inner.new(tag => %opts<tag>)
    }
}

my $p = CallerPkg::invoke(Outer.new, {tag => 'hi'});
my $inner = await $p;
is $inner.^name, 'Outer::Inner', 'nested class resolved from the method-declaring class';
is $inner.tag, 'hi', 'options passed through';
