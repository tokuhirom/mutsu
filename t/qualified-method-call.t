use Test;

plan 13;

# Qualified method call on built-in types
is (-42).Int::abs, 42, 'qualified method call on Int';
is (-42).::Int::abs, 42, 'qualified method call with leading :: on Int';
is "hello".Str::uc, "HELLO", 'qualified method call on Str';

# Qualified method call with parent class
{
    my class Parent {
        method me { self; }
    }
    my class Child is Parent {
        method myself { self.Parent::me(); }
    }
    my $child = Child.new;
    is $child.myself, $child, 'qualified method call dispatches to parent class';
}

# Qualified method call with role
{
    my role R {
        method me { self; }
    }
    my class Consumer does R {
        method myself { self.R::me(); }
    }
    my $consumer = Consumer.new;
    is $consumer.myself, $consumer, 'qualified method call dispatches to role';
}

# Qualified method call with inheritance chain
{
    my class A {
        method foo { "A::foo" }
    }
    my class B is A {
        method foo { "B::foo" }
    }
    my class C is B {
        method get-a-foo { self.A::foo }
        method get-b-foo { self.B::foo }
    }
    my $c = C.new;
    is $c.get-a-foo, "A::foo", 'qualified method call skips to grandparent';
    is $c.get-b-foo, "B::foo", 'qualified method call dispatches to parent';
}

# Qualified method call with role in inheritance
{
    my class Parent {
        method me { self; }
    }
    my role R0 {
        method on_R0 { "R0::on_R0" }
    }
    my role R1 does R0 is Parent {
        method foo { "R1::foo" }
    }
    my class Foo does R1 {
        method foo { "Foo::foo > " ~ self.R1::foo; }
    }
    my class Bar is Foo {
        method bar { "Bar::bar > " ~ self.Foo::foo }
        method via_R1 { "Bar::via_R1 > " ~ self.R1::foo }
        method via_R0 { "Bar::via_R0 > " ~ self.R0::on_R0 }
        method myself { self.Parent::me }
    }
    my $inst = Bar.new;
    is $inst.foo, "Foo::foo > R1::foo", 'qualified call in parent method works';
    is $inst.bar, "Bar::bar > Foo::foo > R1::foo", 'qualified call chain through parent';
    is $inst.via_R1, "Bar::via_R1 > R1::foo", 'indirect qualification to role of parent';
    is $inst.via_R0, "Bar::via_R0 > R0::on_R0", 'indirect qualification to role on role';
    is $inst.myself, $inst, 'indirect qualification to role parent';
    is $inst.R0::on_R0, "R0::on_R0", 'direct qualified call on object';
}

