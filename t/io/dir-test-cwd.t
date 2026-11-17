use Test;

# `dir(:test)` runs its test with the topic bound to each entry's name and,
# as rakudo's `IO::Path.dir` does, `$*CWD` bound to the listed path's own
# `CWD` when it carries one -- else the caller's `$*CWD`, never the listed
# directory itself. That IO::Path is built once per listing (#18003).

plan 7;

my $base = $*TMPDIR.add("mutsu-dir-test-cwd-$*PID");
$base.mkdir;
LEAVE { .unlink for $base.dir; $base.rmdir }
$base.add($_).spurt('x') for <alpha.txt beta.txt gamma.log>;

my @cwds;
my @names = dir($base, :test({ @cwds.push: $*CWD.Str; $_ ~~ /\.txt$/ })).map(*.basename).sort;
is-deeply @names, [<alpha.txt beta.txt>], 'a block test filters by name';
is @cwds.elems, 5, 'the test ran for . and .. and each of the three entries';
ok @cwds.all eq $*CWD.Str, "without its own CWD, the test runs in the caller's \$*CWD";
@cwds = ();
dir(IO::Path.new($base, :CWD($base.Str)), :test({ @cwds.push: $*CWD.Str; True }));
ok @cwds && @cwds.all eq $base.Str, "a path's own CWD is the test's \$*CWD for every entry";

is-deeply dir($base, :test(/\.log$/)).map(*.basename).List, ('gamma.log',), 'a regex test';
is dir($base, :test(True)).elems, 5, 'a True test keeps . and ..';
my $before = $*CWD.Str;
dir(IO::Path.new($base, :CWD($base.Str)), :test({ True }));
is $*CWD.Str, $before, '$*CWD is restored after the listing';
