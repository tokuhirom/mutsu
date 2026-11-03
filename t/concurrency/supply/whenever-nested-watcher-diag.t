use Test;

# TEMPORARY diagnostic (to be deleted): CI shows only the numbers of failed
# tests, so the swallowed exception's text is encoded as failing test numbers:
# char i with code c fails test number 1 + i*256 + c.

my $dir = $*TMPDIR.add("mutsu-diag-nested-" ~ $*PID ~ "-" ~ (^1000000).pick);
LEAVE { try { for $dir.dir -> $d { $d.d ?? (for $d.dir { .unlink }; $d.rmdir) !! $d.unlink }; $dir.rmdir } }
$dir.mkdir;

my @caught;
my $s = supply {
    my %watched;
    sub add-dir(IO::Path $d, :$initial) {
        %watched{$d} = True;
        with $d.watch -> $w {
            whenever $w {
                emit $_;
                my $p = .path.IO;
                add-dir($p) if $p.d && !%watched{$p};
                CATCH { default { @caught.push(.^name ~ '|' ~ .message.lines[0]) } }
            }
        }
        for $d.dir {
            emit IO::Notification::Change.new(path => ~$_, event => FileChanged) unless $initial;
            add-dir($_) if .d && !%watched{$_};
        }
    }
    add-dir($dir, :initial);
}
my $events = Channel.new;
$s.tap: -> $e { $events.send($e.path.IO.basename) };
sleep 0.5;
mkdir $dir.add('svc');
spurt $dir.add('svc/.cro.yml'), "x: 1\n";
my @seen;
my $deadline = now + 5;
while now < $deadline {
    if $events.poll -> $name { @seen.push($name) }
    else { sleep 0.05 }
    last if @seen.grep('.cro.yml');
}
sleep 0.5;
my $msg = @caught ?? @caught[0] !! (@seen.grep('.cro.yml') ?? 'OK' !! 'NONE');
my @chars = $msg.substr(0, 60).ords;
plan 1 + 256 * @chars;
for @chars.kv -> $i, $c {
    for ^256 -> $k {
        ok $k != $c, 'diag';
    }
}
ok True, 'end';
