use Test;

plan 6;

class AnonInvocant {
    method me(::T $:) { T }
}
isa-ok AnonInvocant.new.me, AnonInvocant, 'typed invocant capture binds type object';

class InvocantTypeCheck {
    method x(Int $a:) { 42 }
}
dies-ok { InvocantTypeCheck.new.x() }, 'typed invocant enforces type check';

{
    my $tracker;
    class A {
        method foo {
            my $a = 42;
            method bar { $tracker = $a }
        }
    }
    given A.new {
        .foo;
        .bar;
    }
    is $tracker, 42, 'nested method declared in method body keeps lexical capture';
}

{
    my $tracker;
    class HasMethod {
        $tracker = method foo {};
    }
    isa-ok $tracker, Method, 'method expression returns Method object';
    is $tracker.name, 'foo', 'method expression exposes method name';
}

{
    my $count = 0;
    class ArraySubclass is Array {
        method walk {
            for self { $count++ }
        }
    }
    ArraySubclass.new(1, 2, 3).walk;
    is $count, 3, 'self in Array subclass method is not itemized';
}
