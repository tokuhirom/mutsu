use Test;

# A user `ARGS-TO-CAPTURE` can rewrite the arguments and delegate to the
# default parser through `&*ARGS-TO-CAPTURE`, which is a real callable (not an
# empty stub). Found via the CSS::Minifier distribution's `cssminify` CLI.

plan 4;

my $script = $*TMPDIR.add("mutsu-args-to-capture-{$*PID}.raku");
LEAVE try $script.unlink;
$script.spurt: q:to/RAKU/;
    sub ARGS-TO-CAPTURE(&main, @args --> Capture) {
        my @new = @args.map({ $_ eq '-x' ?? '--extra' !! $_ });
        &*ARGS-TO-CAPTURE(&main, @new)
    }
    sub MAIN($name, Bool :$extra) { say "hello $name extra={$extra // False}" }
    RAKU

my $p = run $*EXECUTABLE, $script, 'world', :out, :err;
is $p.out.slurp(:close).chomp, 'hello world extra=False', 'delegating hook dispatches MAIN';
is $p.exitcode, 0, 'and exits 0';

$p = run $*EXECUTABLE, $script, '-x', 'world', :out, :err;
is $p.out.slurp(:close).chomp, 'hello world extra=True', 'the rewritten arguments are parsed';

$p = run $*EXECUTABLE, $script, :out, :err;
is $p.exitcode, 2, 'a failed dispatch still reports usage and exits 2';
