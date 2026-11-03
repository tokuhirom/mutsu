use Test;

plan 3;

# A pending-DESTROY instance that holds a `but`-mixed clone whose attribute refers
# back to it (instance -> mixin clone -> guard attribute -> instance) is released
# at thread teardown. A debug build used to panic there with
# "Gc::drop strong-count underflow" (a double release of the shared attribute
# store); the release build exited 0 silently, so the process is run and its
# stderr and exit status are checked.
my $code = q:to/CODE/;
role Scoped { has $.guard; method m { 1 } }
class Driver { submethod DESTROY { say "destroy" } }
class Guard {
    has $.driver;
    has $!scoped;
    submethod TWEAK { $!scoped = ($!driver but Scoped).clone(:guard(self)) }
    method scoped { $!scoped }
}
{
    my $g = Guard.new(driver => Driver.new);
    say $g.scoped.m;
}
for ^3 {
    my $g = Guard.new(driver => Driver.new);
    $g.scoped.m;
}
say "end";
CODE

my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
is $proc.exitcode, 0, 'the process exits cleanly';
is $err, '', 'nothing on stderr (no Gc::drop strong-count underflow)';
like $out, / ^ "1\n" .* "end\n" $ /, 'the program ran to the end';
