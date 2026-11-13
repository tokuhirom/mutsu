use Test;

# Supply.Channel on a Proc::Async output stream fetched before .start taps it
# live: the channel receives the output and closes at the end.

plan 3;

my $proc = Proc::Async.new($*EXECUTABLE.Str, '-e', 'say "hello"; note "err"');
my $out = $proc.stdout.Channel;
my $err = $proc.stderr.Channel;
my $done = $proc.start;
my $text = '';
my $reader = start {
    loop {
        my $c;
        last unless so try { $c = $out.receive; True };
        $text ~= $c;
    }
};
await $done;
await $reader;
is $text, "hello\n", 'stdout through the channel';
is $err.list.join, "err\n", 'stderr through its own channel';

my $p = Proc::Async.new('echo', 'yo');
my $ch = $p.stdout.Channel;
$p.start;
is $ch.list.join, "yo\n", '.list of the channel ends when the process does';
