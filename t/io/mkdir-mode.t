use Test;

# `mkdir` and `IO::Path.mkdir` honour their mode argument (Rakudo's
# `mkdir($path, Int() $mode = 0o777)`), through the routine nqp::mkdir shares.

plan 4;

my $dir = $*TMPDIR.add("mutsu-mkdir-mode-{$*PID}");
LEAVE { run 'rm', '-rf', $dir.Str }

mkdir $dir.add('sub').Str, 0o700;
is $dir.add('sub').mode, '0700', 'mkdir sub applies its mode';
$dir.add('meth').mkdir(0o750);
is $dir.add('meth').mode, '0750', 'IO::Path.mkdir applies its mode';

my $f = $dir.add('file');
$f.spurt('x');
my $failure = $f.add('x').mkdir(0o700);
isa-ok $failure.exception, X::IO::Mkdir, 'mkdir under a file fails';
is $failure.exception.message,
    "Failed to create directory '{$f.add('x').absolute}' with mode '0o700': Failed to mkdir: not a directory",
    'the failure names the requested mode';
