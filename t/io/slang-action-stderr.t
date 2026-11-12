use Test;

plan 5;

my $proc = run $*EXECUTABLE, '-It/lib', '-e',
    'use SlangStderr; dp { 1 }', :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);

is $proc.exitcode, 0, 'the slang action succeeds';
is $out, '', 'the action writes no stdout';
is $err, "from the token\nfrom the action\nfrom the handle\n", 'host token and action output reaches stderr';

# An AST dump is analysis, so the slang host must never run for it.
my $dump = run $*EXECUTABLE, '--dump-ast', '-It/lib', '-e',
    'use SlangStderr; dp { 1 }', :out, :err;
$dump.out.slurp(:close);
is $dump.exitcode, 0, 'the AST dump succeeds';
is $dump.err.slurp(:close), '', 'analysis does not execute the slang';
