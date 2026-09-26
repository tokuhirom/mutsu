use Test;

plan 3;

# Rakudo's implicit MAIN handling is `RUN-MAIN(&MAIN, ...)`, which builds the
# dispatch capture with an `ARGS-TO-CAPTURE` found in scope instead of the
# default command-line parser. Getopt::Long exports one (and App::Lorea
# re-exports it); mutsu's implicit dispatch ignored it, so a program relying
# on it got the default parser's "Usage:" instead.

sub run-main(Str $code, *@args) {
    my $r = run($*EXECUTABLE, '-e', $code, |@args, :out, :err);
    ($r.out.slurp(:close) ~ $r.err.slurp(:close)).trim
}

is run-main(q:to/CODE/, 'a', 'b'),
    sub ARGS-TO-CAPTURE(&main, @args) { \(|@args.reverse, :hooked) }
    sub MAIN(*@pos, Bool :$hooked) { print "@pos[] hooked=$hooked" }
    CODE
    'b a hooked=True', 'a mainline ARGS-TO-CAPTURE builds the MAIN capture';

is run-main(q:to/CODE/, '--x=1'),
    sub ARGS-TO-CAPTURE(&main, @args) { \(|@args) }
    sub MAIN(*@pos) { print "pos=@pos[]" }
    CODE
    'pos=--x=1', 'the hook receives @*ARGS unparsed';

is run-main(q:to/CODE/),
    sub MAIN() { print "default" }
    CODE
    'default', 'without a hook the default parser still runs';
