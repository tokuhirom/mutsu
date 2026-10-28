use Test;

plan 2;

# A module's END phaser resolves routines the module imported, even though
# at exit the running compunit is the main script, which never imported them.
my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e',
    'use EndPhaserImportUser; say "main"', :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
is $out, "main\nend saw: x\nend saw: direct\n", 'END body calls the module\'s imports';
is $err, '', 'no "Unknown function" error at exit';
