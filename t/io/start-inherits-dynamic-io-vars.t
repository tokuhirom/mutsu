use Test;

# A thread inherits its spawner's dynamic IO variables, as every other
# dynamic variable is inherited: the spawned code runs in the spawning
# dynamic scope. mutsu used to rebuild `$*CWD`, `$*TMPDIR`, `$*HOME`,
# `$*SPEC`, `$*EXECUTABLE` and `$*EXECUTABLE-NAME` from the process state on
# every thread clone, so an `indir` (or a plain `my $*CWD = ...`) around the
# spawn was lost -- and rebuilding them cost about a third of the per-task
# clone (#17337).

plan 6;

# The paths are built OUTSIDE the blocks that override the dynamic: a `my
# $*CWD` declaration covers its whole block, so `.IO` inside one would
# resolve against the not-yet-assigned override (rakudo warns).
my $dir  = $*TMPDIR.absolute;
my $path = $dir.IO;
my $tmp  = '/mutsu-inherited-tmpdir'.IO;
my $home = '/mutsu-inherited-home'.IO;

{
    my $*CWD = $path;
    is (await start { $*CWD.absolute }), $dir, 'a plain my $*CWD is inherited';
}

is indir($*TMPDIR, { await start { $*CWD.absolute } }), $*TMPDIR.absolute,
  'indir around a start block binds the thread\'s $*CWD';

{
    my $*TMPDIR = $tmp;
    is (await start { $*TMPDIR.absolute }), '/mutsu-inherited-tmpdir',
      'a my $*TMPDIR is inherited';
}

{
    my $*HOME = $home;
    is (await start { $*HOME.absolute }), '/mutsu-inherited-home',
      'a my $*HOME is inherited';
}

# The defaults still reach a thread that overrides nothing.
is (await start { $*SPEC.^name }), $*SPEC.^name, 'the default $*SPEC reaches a thread';
is (await start { $*EXECUTABLE-NAME }), $*EXECUTABLE-NAME,
  'the default $*EXECUTABLE-NAME reaches a thread';
