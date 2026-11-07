use v6;
use Test;

# `run |@cmd, |%opts` with `%opts<env> = %env`: the splatted hash's element
# arrives as a container reference, which `run` did not look through, so the
# child got an empty environment. Found with App::Ariza t/19 (`try-run`).
# Identical under raku.

plan 3;

my %env = HOME => "/x", FOO => "bar";
my @cmd = 'sh', '-c', 'echo "HOME=$HOME FOO=$FOO"';

my %o = :out; %o<env> = %env;
is (run |@cmd, |%o).out.slurp(:close).trim, 'HOME=/x FOO=bar', 'element assigned after construction';

my %p = :out, :err; %p<env> = %env;
is (run |@cmd, |%p).out.slurp(:close).trim, 'HOME=/x FOO=bar', 'with :err too';

sub try-run(@c, :%env) { my %opts = :out; %opts<env> = %env if %env; (run |@c, |%opts).out.slurp(:close).trim }
is try-run(@cmd, :%env), 'HOME=/x FOO=bar', 'through a wrapper taking :%env';
