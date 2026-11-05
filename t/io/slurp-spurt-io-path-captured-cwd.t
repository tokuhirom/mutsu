use v6;
use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# Found via the cro distribution (Cro::Tools::Link::Editor's test suite,
# t/tools-link-editor.rakutest): `slurp`/`spurt` called as free functions
# (not methods) on an IO::Path constructed before a `chdir` must still
# resolve against the CWD captured at `.IO` construction time, not the CWD
# live at the time `slurp`/`spurt` run.
#
# Everything happens under a temporary directory (#16158): the test used to
# create and remove a directory relative to the cwd, which dirtied the
# checkout and tripped the gate's branch stage.

plan 3;

my $saved-cwd = $*CWD;
my $base = make-temp-dir;
chdir $base;
LEAVE { chdir $saved-cwd }

mkdir 'sub';
my $file = "sub/file.txt".IO;
spurt $file, "before\n";

chdir 'sub';

is slurp($file), "before\n", 'slurp() via a free function resolves an IO::Path captured before chdir';

lives-ok { spurt $file, "after\n" }, 'spurt() via a free function resolves an IO::Path captured before chdir';
chdir $base;
is slurp($file), "after\n", 'the free-function spurt() actually wrote to the pre-chdir path';
