use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers");
use Test::Util;

plan 2;

# A parse error inside a `use`d module used to be misattributed to the
# entry-point script: the CLI always rendered "===SORRY!=== Error while
# compiling <entry-point>" and "at <entry-point>:<line>" using the
# entry-point's own name/source, even when the failure was actually deep
# inside a module file the entry-point transitively `use`d. Since the
# module's line number was almost always out of range for the (usually much
# shorter) entry-point source, the source snippet was silently dropped too.
#
# Root cause: `parse_module_source` (src/runtime/run_modules.rs) parses each
# module file with its own source, so the parser correctly computes
# line/column *relative to that module* -- but nothing recorded which file
# that was, so `error_render::render_error` always fell back to whatever
# `source`/`program_name` the CLI passed in for the top-level entry point.

my $module-lib = make-temp-dir;
my $module-name = 'MutsuBadModForParseErrorLocation';
spurt $module-lib.add("$module-name.rakumod"),
    "unit module $module-name;\n\nsub ok-sub() \{ say 'loaded' }\n\n1 +;\n";

my %got = get_out(
    "use $module-name; say \"unreachable\";",
    '',
    :compiler-args(['-I', $module-lib.Str]),
);

like %got<err>, / 'Error while compiling ' .* $module-name '.rakumod' /,
    "a used module's own parse failure names the module file, not the entry point";
like %got<err>, / 'at ' .* $module-name '.rakumod' .* ':5' <!before \d> /,
    'the reported line number is relative to the module file (its own line 5), not the entry point';
