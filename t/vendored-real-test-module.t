use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 10;

# `MUTSU_REAL_TEST=1` makes `use Test` load the vendored upstream
# `Test.rakumod` (modules/Rakudo-Core/lib/) instead of being recognized as a
# no-op that leaves mutsu's native TAP provider in charge. Step 2 of
# `todo/tickets/vendor-real-test-module.md`: exercise the real module without
# yet swapping the foundation the whole suite stands on.

my $vendored = $*PROGRAM.parent(2).add("modules/Rakudo-Core/lib/Test.rakumod");
ok $vendored.e, 'the upstream Test.rakumod is vendored in the repository';
like $vendored.slurp, /'unit module Test;'/,
    'the vendored file is the upstream module, unrenamed';

# A decisive probe for *which* implementation answered: the module exports its
# own `MONKEY-SEE-NO-EVAL`, which the native provider has no equivalent of. The
# child inherits `%*ENV`, so each half sets the switch it wants.
my $probe = 'use Test; plan 1; is MONKEY-SEE-NO-EVAL(), 1, "module export";';
my $plain = 'use Test; plan 2; ok 1, "a"; is 1+1, 2, "b";';

# --- switch off: the native provider answers ---
%*ENV<MUTSU_REAL_TEST>:delete;

is_run $plain, { status => 0, out => "1..2\nok 1 - a\nok 2 - b\n", err => '' },
    'the native provider emits plain TAP';
# rakudo has no native provider to fall back to -- its `Test` *is* this module --
# so only mutsu can assert the negative half of the probe.
if $*RAKU.compiler.name eq 'mutsu' {
    is_run $probe, { status => 255 },
        'the native provider has no MONKEY-SEE-NO-EVAL export';
} else {
    skip 'no native Test provider to distinguish from', 1;
}

# --- switch on: the vendored upstream module answers ---
%*ENV<MUTSU_REAL_TEST> = '1';

is_run $plain, { status => 0, out => "1..2\nok 1 - a\nok 2 - b\n", err => '' },
    'the vendored module emits the same plain TAP';
is_run $probe, { status => 0, out => /'ok 1 - module export'/ },
    'the vendored module supplies its own exports';

# `prove` reads the exit status, so the vendored module's own END must set it.
is_run 'use Test; plan 2; ok 1, "a"; ok 0, "b";', { status => 1 },
    'a failing assertion exits 1 under the vendored module';
is_run 'use Test; plan 3; ok 1, "a";', { status => 255 },
    'a short plan exits 255 under the vendored module';

# There are two dispatch paths into the native TAP provider, and only one of
# them had the "an imported declaration wins" guard. A source that merely
# *mentions* NativeCall gets its prelude injected, which is enough to send a
# listop call down the other path -- where the native `plan` recorded a plan
# nobody ran against, so `finish()` reported "You planned 2 test, but ran 0"
# on a file whose assertions had all passed.
is_run "# NativeCall\nuse Test; plan 2; ok 1, 'a'; ok 1, 'b';",
    { status => 0, out => "1..2\nok 1 - a\nok 2 - b\n", err => '' },
    'the module answers on the fallback dispatch path too';

# `is test-assertion` (news/2026-08/test-assertion-trait-is-not-introspectable.md):
# the vendored module's own `trait_mod:<is>(Routine:D, :$test-assertion!)`
# mixes an introspectable `is-test-assertion` role onto the routine so its
# `callframe`-walking backtrace can blame the CALLER's line, not its own. That
# requires mutsu's parser to actually reach the module's user trait handler,
# and the mixed-in role to survive into `callframe(N).code` on every
# rebuild -- both were previously broken, always reporting the assertion
# helper's own line instead.
is_run
    "use Test;\nplan 1;\nsub foo-ok() is test-assertion \{ flunk \"foo-ok\" \}\nfoo-ok;\n",
    { status => 1, err => /'Failed test ' (\N* \n \N*)? 'at ' \N* ' line 4'/ },
    'is test-assertion blames the call site line under the vendored module';
