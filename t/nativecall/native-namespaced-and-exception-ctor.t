use Test;

# §D state-ownership: the VM constructs `::`-namespaced classes and built-in
# exception types natively (no interpreter `.new` bounce), behaving identically
# to the interpreter's generic constructor.

plan 16;

# --- user `A::B` namespaced class (declared attributes) ---
{
    class A::B { has $.x; has Int $.y = 5; }
    my $o = A::B.new(x => 10);
    is $o.x, 10, 'namespaced class: provided attr';
    is $o.y, 5, 'namespaced class: default attr';
    is $o.^name, 'A::B', 'namespaced class: name';
}

# --- deeper namespace ---
{
    class P::Q::R { has $.v = 42; }
    is P::Q::R.new.v, 42, 'three-level namespace default';
    is P::Q::R.new(v => 1).v, 1, 'three-level namespace provided';
}

# --- built-in exception type: attribute-bag construction ---
{
    my $e = X::AdHoc.new(payload => "boom");
    is $e.payload, "boom", 'X::AdHoc payload stored';
    is $e.message, "boom", 'X::AdHoc message from payload';
    is $e.^name, 'X::AdHoc', 'X::AdHoc name';
    ok $e ~~ X::AdHoc, 'X::AdHoc isa X::AdHoc';
    ok $e ~~ Exception, 'X::AdHoc isa Exception';
}

# --- nested built-in exception type ---
{
    my $t = X::TypeCheck::Binding.new(got => 1, expected => Str);
    is $t.^name, 'X::TypeCheck::Binding', 'nested exception name';
    ok $t ~~ X::TypeCheck, 'nested exception isa parent';
    is $t.got, 1, 'nested exception got attr';
}

# --- user exception with a `message` method (materialize at construction) ---
{
    my class X::My::Boom is Exception {
        has $.detail;
        method message { "boom: " ~ $.detail }
    }
    my $e = X::My::Boom.new(detail => "kaboom");
    is $e.message, "boom: kaboom", 'user exception message method materialized';
    is $e.detail, "kaboom", 'user exception attr';
}

# --- throwing a natively-constructed exception still works ---
{
    my $caught;
    {
        CATCH { default { $caught = .message } }
        die X::AdHoc.new(payload => "thrown");
    }
    is $caught, "thrown", 'natively-constructed exception throws & catches';
}
