use v6;
use Test;

# Found via the cro distribution (Cro::Tools::Link::Editor's test suite,
# t/tools-link-editor.rakutest): `slurp`/`spurt` called as free functions
# (not methods) on an IO::Path constructed before a `chdir` must still
# resolve against the CWD captured at `.IO` construction time, not the CWD
# live at the time `slurp`/`spurt` run.

my $dir = 'tmp-slurp-spurt-cwd-test'.IO;
$dir.mkdir;
LEAVE { rm-dir-recursive($dir) }

my $file = "tmp-slurp-spurt-cwd-test/file.txt".IO;
spurt $file, "before\n";

my $saved-cwd = $*CWD;
chdir $dir;
LEAVE { chdir $saved-cwd }

is slurp($file), "before\n", 'slurp() via a free function resolves an IO::Path captured before chdir';

lives-ok { spurt $file, "after\n" }, 'spurt() via a free function resolves an IO::Path captured before chdir';
chdir $saved-cwd;
is slurp($file), "after\n", 'the free-function spurt() actually wrote to the pre-chdir path';

sub rm-dir-recursive($d) {
    return unless $d.e;
    for $d.dir -> $entry {
        $entry.d ?? rm-dir-recursive($entry) !! $entry.unlink;
    }
    $d.rmdir;
}

done-testing;
