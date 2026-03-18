use Test;

plan 3;

my @events;

class Foo {
    submethod DESTROY { @events.push("foo") }
}

my $foo = Foo.new;
$foo = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["foo"], "DESTROY runs when the last reference is dropped";

class Base {
    submethod DESTROY { @events.push("base") }
}

class Child is Base {
}

my $child = Child.new;
$child = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["foo", "base"], "DESTROY submethods walk MRO (DESTROYALL)";

class ChildWithDestroy is Base {
    submethod DESTROY { @events.push("child") }
}

my $child_with_destroy = ChildWithDestroy.new;
$child_with_destroy = Nil;
quietly $*VM.request-garbage-collection;
is-deeply @events, ["foo", "base", "child", "base"], "DESTROYALL calls DESTROY on each class in MRO";
