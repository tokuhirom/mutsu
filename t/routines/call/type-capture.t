use Test;

plan 3;

sub declare_cap_type(::T $x) {
    my T $y = 4.2;
    $y
}

lives-ok { declare_cap_type(3.3) }, "captured type is available in local variable declaration";
is-approx declare_cap_type(3.3), 4.2, "captured type declaration accepts matching values";
dies-ok { declare_cap_type(42) }, "captured type declaration enforces the captured constraint";
