use Test;

plan 3;

{
    my $called = 0;
    sub next-num { $called++; 42 }

    class Counter {
        has $.value = next-num();
    }

    my Counter $c .= new;
    is $called, 1, 'attribute default side effects are preserved';
    is $c.value, 42, 'attribute default value is applied';
}

{
    class Foo {
        has Int $.a;
        BEGIN ::?CLASS.^attributes.head.set_build: -> |c { "foo" }
    }

    dies-ok { Foo.new }, 'typed attribute constraint is checked after set_build override';
}
