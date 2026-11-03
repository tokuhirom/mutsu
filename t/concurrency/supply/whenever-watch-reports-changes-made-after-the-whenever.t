use Test;

# rakudo taps a `whenever`'s source at the `whenever` statement, so a change a
# `supply` block body makes AFTER the `whenever` (a file written, a nested
# directory listed and then watched) is reported. mutsu tapped when the body
# returned and a filesystem watcher, which starts at its first tap, moved its
# baseline past the change: the Cro `watch-recursive` shape lost a file created
# between the listing and the tap (#16198, CI only).

plan 4;

sub fresh-dir(Str $tag) {
    my $dir = $*TMPDIR.add("mutsu-whenever-watch-$tag-" ~ $*PID ~ "-" ~ (^1000000).pick);
    $dir.mkdir;
    $dir
}
sub cleanup($dir) { try { .unlink for $dir.dir; $dir.rmdir } }

{
    my $dir = fresh-dir('body');
    my $s = supply {
        whenever $dir.watch -> $e { emit $e.path.IO.basename }
        sleep 0.3;
        spurt $dir.add('late.txt'), 'x';
        sleep 0.3;
    }
    my @got;
    $s.tap: -> $n { @got.push($n) };
    sleep 1.5;
    ok @got.grep('late.txt').elems >= 1, 'a file written by the block body after the whenever is reported';
    cleanup($dir);
}

# The same through a nested sub, whose whenever runs on a worker (the
# recursive-watcher shape).
{
    my $dir = fresh-dir('nested');
    my $s = supply {
        sub add-watch($d) {
            with $d.watch -> $w { whenever $w { emit .path.IO.basename } }
            sleep 0.3;
            spurt $d.add('late.txt'), 'x';
        }
        add-watch($dir);
        sleep 0.3;
    }
    my @got;
    $s.tap: -> $n { @got.push($n) };
    sleep 1.5;
    ok @got.grep('late.txt').elems >= 1, 'a change made after a nested sub registered the whenever is reported';
    cleanup($dir);
}

# A plain watch still starts at its first tap: nothing from before it.
{
    my $dir = fresh-dir('plain');
    my $w = $dir.watch;
    spurt $dir.add('before.txt'), 'x';
    sleep 0.4;
    my @got;
    my $tap = $w.tap: -> $e { @got.push($e.path.IO.basename) };
    sleep 0.3;
    spurt $dir.add('after.txt'), 'x';
    sleep 0.6;
    $tap.close;
    nok @got.grep('before.txt'), 'a change before the first tap is not reported';
    ok @got.grep('after.txt').elems >= 1, 'a change after it is';
    cleanup($dir);
}
