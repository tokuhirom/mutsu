use v6;
use Test;

# A warning raised while parsing a `use`d module (e.g. a "Duplicate 'is
# export' trait" warning) used to be printed with the *importer's* current
# execution line, not the module's own file and offending line --
# write_warn_to_stderr (src/runtime/runtime_output.rs) appended a
# current-execution backtrace instead of consulting the warning's own
# recorded location. Fixed by having parser::add_parse_warning bake a
# "\n    at FILE:LINE" suffix (using parser_source_file(), which is swapped
# per compilation unit, not parser_program_path(), which stays pinned to
# the top-level script) directly into the warning message, and having
# write_warn_to_stderr recognize that suffix and skip its own backtrace.
#
# Reuses the fixture module from t/module-parse-warning-once.t (deliberately
# duplicates `is export` on line 5 to trigger the warning).

plan 4;

my $code = 'use ModuleParseWarningOnceFixture; say module-parse-warning-once-hello();';
my $proc = run($*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err);
my $out = $proc.out.slurp(:close).trim;
my $err = $proc.err.slurp(:close);

is $out, 'hi', 'the module still loads and runs correctly';
ok $err.contains('ModuleParseWarningOnceFixture.rakumod:5'),
    'the warning names the MODULE\'s own file and its actual offending line (5), not the importer\'s';
nok $err.contains('-e line 1'),
    'the warning does NOT misattribute the location to the importer\'s -e line 1';
nok $err.contains("\n  in block <unit> at -e"),
    'no spurious current-execution backtrace is appended on top of the warning\'s own location';
