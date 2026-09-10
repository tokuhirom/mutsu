use Test;

plan 3;

# `use Test::*` for a module that genuinely does not exist anywhere on the
# search path used to be silently tolerated: no error, no warning, just a
# quiet no-op (src/runtime/runtime_module.rs, use_module_with_tags_inner's
# `module.starts_with("Test::")` branch). That leniency exists for real
# compatibility reasons (roast and bundled suites `use` test-only helper
# modules mutsu does not vendor) and must stay, but the total silence could
# mask a genuinely missing dependency (a typo, not a deliberately-unvendored
# helper) behind a test file that quietly ran zero of its assertions.

my $code = 'use Test::ThisModuleDoesNotExistAnywhere; use Test; ok 1, "line after the missing use still runs"; done-testing';
my $proc = run $*EXECUTABLE.absolute, '-e', $code, :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

ok $err.contains('Test::ThisModuleDoesNotExistAnywhere'),
    'missing Test::* use now emits a stderr note naming the module';
ok $out.contains('ok 1'),
    'the missing use stays non-fatal -- the rest of the script still runs';
is $proc.exitcode, 0, 'exits successfully despite the missing Test::* module';
