use Test;
use NativeCall;

# `%*ENV` is an ordinary hash, as in rakudo (#11241): a write or delete is never
# mirrored into the C-level process environment (`setenv` races with C code
# reading the environment on another thread), and a child started by `run`,
# `shell` or `Proc::Async` gets the hash as its *whole* environment.

plan 9;

sub getenv(Str --> Str) is native {}

%*ENV<MUTSU_11241_PROBE> = 'x';
nok getenv('MUTSU_11241_PROBE').defined, 'a %*ENV write does not reach the C environment';
is run('sh', '-c', 'echo "child=$MUTSU_11241_PROBE"', :out).out.slurp(:close).trim,
    'child=x', 'run() child sees the %*ENV write';
is shell('echo "child=$MUTSU_11241_PROBE"', :out).out.slurp(:close).trim,
    'child=x', 'shell() child sees the %*ENV write';

# A key present in the process environment at startup and then deleted from
# `%*ENV` must be gone for the program and for its children.
my $script = q:to/END/;
    %*ENV<MUTSU_11241_DEL>:delete;
    say %*ENV<MUTSU_11241_DEL>:exists;
    say %*ENV<MUTSU_11241_DEL>.defined;
    print run('sh', '-c', 'echo "run=[${MUTSU_11241_DEL-unset}]"', :out).out.slurp(:close);
    print shell('echo "shell=[${MUTSU_11241_DEL-unset}]"', :out).out.slurp(:close);
    my $p = Proc::Async.new('sh', '-c', 'echo "async=[${MUTSU_11241_DEL-unset}]"');
    my $out = '';
    $p.stdout.tap({ $out ~= $_ });
    await $p.start;
    print $out;
    END
my %child-env = %*ENV;
%child-env<MUTSU_11241_DEL> = 'inherited';
my @lines = run($*EXECUTABLE, '-e', $script, :env(%child-env), :out).out.slurp(:close).lines;
is @lines[0], 'False', 'a deleted startup variable no longer :exists';
is @lines[1], 'False', 'a deleted startup variable reads as undefined';
is @lines[2..4].join(' '), 'run=[unset] shell=[unset] async=[unset]',
    'a deleted startup variable is not inherited by run/shell/Proc::Async children';

# An explicit :env is the child's whole environment too.
is run('sh', '-c', 'echo "[${MUTSU_11241_PROBE-unset}][$ONLY]"', :env{ ONLY => 1, PATH => %*ENV<PATH> }, :out)
    .out.slurp(:close).trim, '[unset][1]', 'run(:env) replaces the whole environment';

# `$*SPEC.path` follows `%*ENV<PATH>`.
{
    temp %*ENV<PATH> = '/mutsu-11241-a:/mutsu-11241-b';
    is-deeply $*SPEC.path.list, ('/mutsu-11241-a', '/mutsu-11241-b'), '$*SPEC.path reads %*ENV<PATH>';
}
{
    temp %*ENV<PATH> = '';
    is-deeply $*SPEC.path.list, (), '$*SPEC.path is empty for an empty %*ENV<PATH>';
}
