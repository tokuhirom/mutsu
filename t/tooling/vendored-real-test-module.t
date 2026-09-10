use v6;
use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 8;

# `use Test` loads the vendored upstream `Test.rakumod`
# (modules/Rakudo-Core/lib/). It is the only provider there is: mutsu's native
# TAP provider was retired on 2026-09-10 (#7566), taking the `MUTSU_REAL_TEST`
# escape hatch with it. This file pins that the vendored module is what
# answers, and that the behaviours the retirement depended on are still there.

my $vendored = "modules/Rakudo-Core/lib/Test.rakumod".IO;
ok $vendored.e, 'the upstream Test.rakumod is vendored in the repository';
like $vendored.slurp, /'unit module Test;'/,
    'the vendored file is the upstream module, unrenamed';

# A decisive probe for *which* implementation answered: the module exports its
# own `MONKEY-SEE-NO-EVAL`, which no reimplementation of it ever had.
my $probe = 'use Test; plan 1; is MONKEY-SEE-NO-EVAL(), 1, "module export";';
my $plain = 'use Test; plan 2; ok 1, "a"; is 1+1, 2, "b";';

is_run $plain, { status => 0, out => "1..2\nok 1 - a\nok 2 - b\n", err => '' },
    'the vendored module emits plain TAP';
is_run $probe, { status => 0, out => /'ok 1 - module export'/ },
    'the vendored module supplies its own exports';

# `prove` reads the exit status, so the vendored module's own END must set it.
is_run 'use Test; plan 2; ok 1, "a"; ok 0, "b";', { status => 1 },
    'a failing assertion exits 1 under the vendored module';
is_run 'use Test; plan 3; ok 1, "a";', { status => 255 },
    'a short plan exits 255 under the vendored module';

# There used to be two dispatch paths into a native TAP provider, and only one
# of them had the "an imported declaration wins" guard. A source that merely
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
