use Test;

plan 8;

role SampleRole {
    method sample_method { 42 }
}

my $obj = SampleRole.new;
ok $obj.defined, "roles can be instantiated";
ok $obj ~~ SampleRole, "role instance smartmatches against role";
is $obj.WHAT.gist, "(SampleRole)", "role instance WHAT reports role name";

role ParaRole[$x] {
    method get_x { $x }
}

my $obj2 = ParaRole[42].new;
ok $obj2 ~~ ParaRole, "parametric role instance smartmatches against base role";
ok $obj2 ~~ ParaRole[42], "parametric role instance smartmatches against parameterized role";
is $obj2.get_x, 42, "parameterized role argument is available in methods";

role ParaRole2[$x, $y] {
    method sum { $x + $y }
}
is ParaRole2[4, 5].new.sum, 9, "multi-argument parameterized role instantiates correctly";

role NotNewPun {
    method x { 69 }
}
is NotNewPun.x, 69, "role type object puns for non-.new methods";
