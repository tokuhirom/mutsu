use Test;

# A literal positional constraint on a MAIN candidate (e.g. `MAIN('info', ...)`,
# the shape zef's subcommand CLI uses) must match the argument by value. A
# literal candidate must not greedily match a call whose first argument differs.

plan 7;

my $prog = $*TMPDIR.add("main-lit-{$*PID}.raku");
$prog.spurt: q:to/RAKU/;
    multi sub MAIN('info', *@ids)        { say "INFO " ~ @ids.join(',') }
    multi sub MAIN('locate', Str $path)  { say "LOCATE $path" }
    multi sub MAIN(*@rest)               { say "REST " ~ @rest.join(',') }
    RAKU

sub run-prog(*@args) {
    my $proc = run($*EXECUTABLE, $prog, @args, :out, :err);
    $proc.out.slurp(:close).chomp;
}

is run-prog('info'),            'INFO ',         'literal info, no extra args';
is run-prog('info', 'A', 'B'),  'INFO A,B',      'literal info collects slurpy identities';
is run-prog('locate', '/x/y'),  'LOCATE /x/y',   'literal locate with required Str positional';
is run-prog('other', 'x'),      'REST other,x',  'non-matching first arg falls to slurpy fallback';
is run-prog('xinfo'),           'REST xinfo',    'partial literal match does not match literal candidate';
is run-prog(),                  'REST ',         'no args -> slurpy fallback';
is run-prog('locate'),          'REST locate',   'locate without its required path falls to fallback';

$prog.unlink;
