use Test;

# ADR-0017: an error *parsing the option list* follows rakudo -- the message and
# usage go to stderr, and the process exits 0. A program-level failure (a
# program file that cannot be opened) is unchanged and still exits 1.
#
# Before this, an unrecognised switch fell through to "this must be the program
# file" and died with `Could not open --nosucharg=foo`, exit 1.

plan 12;

my $script = $*TMPDIR.child("mutsu-cli-opt-{$*PID}.raku");
$script.spurt: "say 42\n";
LEAVE try $script.unlink;

sub mutsu(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

my ($status, $out, $err) = mutsu('--nosucharg=foo', 'foo.raku');
is $status, 0, 'an unknown long option exits 0';
is $out, '', '... writes nothing to stdout';
ok $err.starts-with("Illegal option --nosucharg\n"),
    '... and names it on stderr without its =value';

($status, $out, $err) = mutsu('-z');
is $status, 0, 'an unknown short option exits 0';
ok $err.starts-with("No such option -z\n"), '... with rakudo\'s short-option wording';

# A malformed negation keeps its message on stdout (which is what
# roast/S19-command-line-options/04-negation.t asks for) and exits 0.
($status, $out, $err) = mutsu('-/hv');
is $status, 0, 'a malformed negation exits 0';
like $out, /'SORRY' .+ 'cannot be negated'/, '... reporting it on stdout';
is $err, '', '... and leaving stderr empty';

# `--` ends the switches, so the program file may begin with a dash.
($status, $out, $err) = mutsu('--', $script.absolute);
is $status, 0, '-- ends the switch list';
is $out, "42\n", '... and what follows is the program file';

# A program file that cannot be opened is NOT an option error: still exit 1.
($status, $out, $err) = mutsu('no-such-file-here.raku');
is $status, 1, 'an unopenable program file still exits 1';
ok $err.contains('Could not open no-such-file-here.raku'),
    '... reporting it on stderr';
