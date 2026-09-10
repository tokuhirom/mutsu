use Test;

# rakudo spells the Pod renderer selection `--doc=module`, meaning
# `Pod::To::[module]`. mutsu only had the bare `--doc`, so the whole
# `--doc=Text` token fell through to the "this is the program file" branch and
# died with "Could not open --doc=Text" -- which is what
# `roast/S26-documentation/02-paragraph.t`'s last assertion (an `is_run` with
# `:compiler-args['--doc=Text']`) saw.

plan 4;

my $pod-file = $*TMPDIR.child("mutsu-doc-renderer-{$*PID}.raku");
$pod-file.spurt: qq:to/SRC/;
    =begin pod
    Hello E<alpha>
    =end pod
    SRC
LEAVE try $pod-file.unlink;

sub mutsu(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

my ($status, $out, $err) = mutsu('--doc=Text', $pod-file.absolute);
is $status, 0, '--doc=Text runs the Pod renderer';
is $out.trim, 'Hello α', '... and renders the same text --doc does';

# An unknown renderer names a module mutsu does not have; rakudo reports that as
# a compile-time "Could not find Pod::To::...", not as a missing program file.
($status, $out, $err) = mutsu('--doc=Nonesuch', $pod-file.absolute);
ok $err.contains('Could not find Pod::To::Nonesuch'),
    'an unknown renderer names the module it could not find';
nok $err.contains('Could not open'),
    '... rather than treating the option as the program file';
