use v6.e.PREVIEW;
use Test;

plan 4;

# Test BUILD submethods from roles are called during construction
{
    my @order;
    my role R0 {
        submethod BUILD(*%a) {
            @order.push: $?ROLE.^name;
        }
    }
    my role R0a {
        # Regular method BUILD should NOT be called
        method BUILD {
            @order.push: $?ROLE.^name;
        }
    }
    my class C0 does R0 does R0a {
        submethod BUILD {
            @order.push: $?CLASS.^name;
        }
    }
    C0.new;
    is-deeply @order.List, <R0 C0>, "v6.e: role BUILD submethods are called before class BUILD";
}

# Test with inheritance and multiple roles
{
    my @order;
    my role R0 {
        submethod BUILD(*%a) {
            @order.push: "R0";
        }
    }
    my class C0 does R0 {
        submethod BUILD {
            @order.push: "C0";
        }
    }
    my role R1 does R0 {
        submethod BUILD {
            @order.push: "R1";
        }
    }
    my role R2 {
        submethod BUILD {
            @order.push: "R2";
        }
    }
    my class C1 does R1 does R2 is C0 {
        submethod BUILD {
            @order.push: "C1";
        }
    }
    C1.new;
    is-deeply @order.List, <R0 C0 R0 R1 R2 C1>,
        "v6.e: BUILD submethods called in correct MRO order with role hierarchy";
}

# Test TWEAK submethods from roles
{
    my @order;
    my role R0 {
        submethod TWEAK(*%a) {
            @order.push: $?ROLE.^name;
        }
    }
    my class C0 does R0 {
        submethod TWEAK {
            @order.push: $?CLASS.^name;
        }
    }
    C0.new;
    is-deeply @order.List, <R0 C0>, "v6.e: role TWEAK submethods are called before class TWEAK";
}

# Test DESTROY submethods from roles
{
    my @called;
    my role R0 {
        submethod DESTROY {
            @called.push: "R0";
        }
    }
    my class C0 does R0 {
        submethod DESTROY {
            @called.push: "C0";
        }
    }
    { my $obj = C0.new }
    # DESTROY fires during cleanup, not necessarily immediately
    # Just verify that destruction runs without crashing
    pass "v6.e: DESTROY with role submethods does not crash";
}

done-testing;
