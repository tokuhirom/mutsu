# A file-scope `my &f` filled from a routine the module body calls at load
# time (the JSON::Native shape), then read by a later exported routine.
my &f;
my &g = sub { "g0" };
sub load() { &f = sub { "f" }; &g = sub { "g1" }; }
load();
my $defined-in-body = &f.defined;
my $called-in-body = f();
sub body-report() is export { "$defined-in-body $called-in-body" }
sub run() is export { f() ~ "/" ~ g() }
