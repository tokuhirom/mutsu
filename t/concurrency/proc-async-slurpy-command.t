use Test;

plan 3;

# Proc::Async.new's command is the slurpy `*@args`, so a List among its
# positionals contributes its elements. App::Lorea's test builds the command
# as `$*EXECUTABLE, '-I' «~« INCLUDE, './bin/lorea', |@args`; the hyper
# result used to reach the child as ONE argument ("-Ia -Ib").

sub argv(*@cmd) {
    my $p = Proc::Async.new: |@cmd;
    my $out = '';
    react {
        whenever $p.stdout { $out ~= $_ }
        whenever $p.start { done }
    }
    $out
}

my @dirs = <a b>;
is argv($*EXECUTABLE, '-e', 'print @*ARGS.join("|")', '-I' «~« @dirs, 'x'),
    '-Ia|-Ib|x', 'a List positional is flattened into separate arguments';

is Proc::Async.new('echo', ('p', 'q'), 'r').command.join('|'), 'echo|p|q|r',
    '.command reports the flattened argument list';

is Proc::Async.new('echo', $('p', 'q'), 'r').command.elems, 3,
    'an itemized List stays one argument';
