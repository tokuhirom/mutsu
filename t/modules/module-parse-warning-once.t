use v6;
use Test;

# A module's parse warning (e.g. a duplicate 'is export' trait) used to be
# printed TWICE: once during the importer's parse-time export scan
# (parser::stmt::simple::module_exports::extract_exported_names), and again
# when the `use` statement actually loads the module at run time
# (Interpreter::parse_module_source). Rakudo -- and a correctly-behaving
# mutsu -- prints it once. See the (now-closed)
# todo/tickets/module-parse-warning-reported-twice.md for the investigation.
#
# The fixture module deliberately duplicates `is export` on a sub to trigger
# the "Duplicate 'is export' trait" parser warning.

plan 3;

my $code = 'use ModuleParseWarningOnceFixture; say module-parse-warning-once-hello();';

sub run-fixture() {
    my $proc = run($*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err);
    (
        out => $proc.out.slurp(:close).trim,
        err => $proc.err.slurp(:close),
    );
}

my %first = run-fixture();
is %first<out>, 'hi', 'the module still loads and runs correctly (cold precompilation cache)';
is +(%first<err>.comb(/'Duplicate' \s+ '\'is export\' trait'/)),
    1,
    q<the "Duplicate 'is export' trait" warning is printed exactly once (cold cache)>;

# The precompilation cache replays a module's recorded parse warnings on a
# hit (see precomp::ParseEffects); run a second, independent process so the
# on-disk cache -- written by the first process above -- is warm, and check
# the duplicate is not reintroduced by the replay path either.
my %second = run-fixture();
is +(%second<err>.comb(/'Duplicate' \s+ '\'is export\' trait'/)),
    1,
    q<...and still exactly once on a warm precompilation cache>;
