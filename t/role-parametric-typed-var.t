use Test;

plan 8;

# Type parameter used as type constraint in role body variable declaration
{
    my role R[::T] {
        my T $v .= new;
        method get-v { $v }
    }
    my class C does R[Int] { }
    is C.get-v, 0, "my T \$v .= new; with T=Int gives Int.new (0)";
}

# Type parameter used in role method
{
    my role R[::T] {
        method type-name { T.^name }
    }
    my class CInt does R[Int] { }
    my class CStr does R[Str] { }
    is CInt.type-name, "Int", "type param resolves to Int in method";
    is CStr.type-name, "Str", "type param resolves to Str in method";
}

# Role body non-method statements are deferred for parameterized roles
{
    my $outer = 0;
    my role R[::T] {
        $outer++;
    }
    is $outer, 0, "parameterized role body stmts are deferred until composition";
}

{
    my role R1[::T] { }
    lives-ok { my R1 of Int $x = R1[Int].new }, "typed variable declaration accepts prefix 'of'";
    dies-ok { my R1 of Int $x = R1[Str].new }, "typed variable declaration enforces prefix 'of'";
}

{
    my role R2[::T] { }
    lives-ok { my R2 of R2 of Int $x = R2[R2[Int]].new }, "nested parameterized role constraint accepts matching role instance";
    throws-like { EVAL 'role ABCD[EFGH] { }' }, X::Parameter::InvalidType, "undefined bare type in role parameter list dies";
}
