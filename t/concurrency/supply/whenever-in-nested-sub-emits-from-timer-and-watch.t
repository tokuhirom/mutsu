use Test;

# `emit` written in a `sub` that a `supply` block calls (so the parser's
# `$emitter.emit` rewrite does not reach it) still emits into that block's
# supply when the `whenever` source is a native one whose callbacks run on a
# worker: `Supply.interval` and `IO::Path.watch`. cro's
# `Cro::Tools::Services.services` (`watch-recursive`) is built this way.

plan 5;

# A Supplier source took this path already; pin it next to the native ones.
{
    my $src = Supplier.new;
    my $s = supply { sub add($d) { whenever $d { emit $_ } }; add($src.Supply) };
    my @got;
    $s.tap: -> $e { @got.push($e) };
    sleep 0.1;
    $src.emit(1);
    $src.emit(2);
    sleep 0.3;
    is-deeply @got, [1, 2], 'a Supplier source emits through a nested sub';
}

{
    my $s = supply { sub add() { whenever Supply.interval(0.05) { emit $_; done if $_ == 2 } }; add() };
    my @got;
    my $done = Promise.new;
    $s.tap: -> $e { @got.push($e) }, done => { $done.keep };
    await Promise.anyof($done, Promise.in(5));
    is-deeply @got, [0, 1, 2], 'Supply.interval emits through a nested sub and `done` ends it';
}

# The recursive-watcher shape: a nested sub adds a watcher per directory and a
# directory created later gets one too, whose events are emitted as well.
{
    my $dir = $*TMPDIR.add("mutsu-nested-emit-" ~ $*PID ~ "-" ~ (^1000000).pick);
    LEAVE { try { for $dir.dir -> $d { $d.d ?? (for $d.dir { .unlink }; $d.rmdir) !! $d.unlink }; $dir.rmdir } }
    $dir.mkdir;

    my $s = supply {
        my %watched;
        sub add-dir(IO::Path $d, :$initial) {
            %watched{$d} = True;
            with $d.watch -> $w {
                whenever $w {
                    emit $_;
                    my $p = .path.IO;
                    add-dir($p) if $p.d && !%watched{$p};
                    CATCH { default { } }
                }
            }
            # What was created before the watcher existed is reported by a
            # listing, as cro's `watch-recursive` does.
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
    ok @seen.grep('svc'), 'the new directory itself is reported';
    ok @seen.grep('.cro.yml'), 'a file created inside it is reported by the nested watcher';
}

# An `emit` outside every supply still raises, as before.
{
    sub bare-emit { emit 1 }
    throws-like { bare-emit() }, X::ControlFlow, 'emit without a supply is still refused';
}
