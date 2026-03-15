use Test;

plan 3;

my @events;

class Foo {
    submethod DESTROY { @events.push("foo") }
}

my $foo = Foo.new;
$foo = Nil;
is-deeply @events, ["foo"], "DESTROY runs when the last reference is dropped";

class Base {
    submethod DESTROY { @events.push("base") }
}

class Child is Base {
}

my $child = Child.new;
$child = Nil;
is-deeply @events, ["foo"], "DESTROY submethods are not inherited";

class ChildWithDestroy is Base {
    submethod DESTROY { @events.push("child") }
}

my $child_with_destroy = ChildWithDestroy.new;
$child_with_destroy = Nil;
is-deeply @events, ["foo", "child"], "a subclass DESTROY runs when declared on that class";
