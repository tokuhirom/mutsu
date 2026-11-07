use Test;

# Found through Code::Coverage 0.0.9: `Code::Coverage!cover` does
# `temp %*ENV; %*ENV<MVM_COVERAGE_LOG> := $path; run ...` once per runner and
# run, and from the second call on the key read back as Nil, so the child
# process never got it. A `temp %*ENV` whose scope stored a key did not
# publish the restored hash back to the process-level stash.

plan 9;

{
    temp %*ENV;
    %*ENV<MUTSU_T_K1> = "a";
    is %*ENV<MUTSU_T_K1>, "a", "a store inside temp %*ENV is visible";
}
nok %*ENV<MUTSU_T_K1>.defined, "the key is gone after the scope";

%*ENV<MUTSU_T_K2> = "z";
is %*ENV<MUTSU_T_K2>, "z", "a store after a temp %*ENV scope sticks";
ok (%*ENV<MUTSU_T_K2>:exists), "and exists";

my @seen;
for 1..3 {
    temp %*ENV;
    %*ENV<MUTSU_T_K3> := "v$_";
    @seen.push: %*ENV<MUTSU_T_K3>;
}
is @seen, <v1 v2 v3>, "every pass of a loop sees its own temporized key";
nok %*ENV<MUTSU_T_K3>.defined, "and none leaks out";

sub with-env($v) {
    temp %*ENV;
    %*ENV<MUTSU_T_K4> := $v;
    %*ENV<MUTSU_T_K4>;
}
is with-env("x"), "x", "first call of a sub using temp %*ENV";
is with-env("y"), "y", "second call of it";
is %*ENV<MUTSU_T_K2>, "z", "an earlier key survives the scopes";

