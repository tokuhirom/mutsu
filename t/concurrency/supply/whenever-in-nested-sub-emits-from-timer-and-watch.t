use Test;

# `emit` written in a `sub` that a `supply` block calls (so the parser's
# `$emitter.emit` rewrite does not reach it) still emits into that block's
# supply when the `whenever` source is a native one whose callbacks run on a
# worker: `Supply.interval` and `IO::Path.watch`. cro's
# `Cro::Tools::Services.services` (`watch-recursive`) is built this way.

plan 25;

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

    my $diag = Channel.new;
    my $s = supply {
        my %watched;
        sub add-dir(IO::Path $d, :$initial) {
            $diag.send('add');
            %watched{$d} = True;
            with $d.watch -> $w {
                whenever $w {
                    $diag.send('body');
                    emit $_;
                    my $p = .path.IO;
                    $diag.send('isdir') if $p.d;
                    add-dir($p) if $p.d && !%watched{$p};
                    CATCH { default { $diag.send('caught'); $diag.send('msg:' ~ .^name ~ '|' ~ .message.lines[0].substr(0, 60)) } }
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
    my @seen;
    my $deadline = now + 10;
    # Wait for the directory's own event, then give the watcher the new
    # directory got a moment to register before the file appears in it: a file
    # created before it exists would only be found by the listing (which the
    # test is not about), and a watcher that starts after the file is created
    # misses it.
    while now < $deadline {
        if $events.poll -> $name { @seen.push($name) }
        else { sleep 0.05 }
        last if @seen.grep('svc');
    }
    sleep 1;
    spurt $dir.add('svc/.cro.yml'), "x: 1\n";
    while now < $deadline {
        if $events.poll -> $name { @seen.push($name) }
        else { sleep 0.05 }
        last if @seen.grep('.cro.yml');
    }
    ok @seen.grep('svc'), 'the new directory itself is reported';
    my $reported = so @seen.grep('.cro.yml');
    diag "events seen: {@seen.raku}; tmpdir {$*TMPDIR}; dir listing {$dir.dir.map(*.basename).raku} / {$dir.add('svc').dir.map(*.basename).raku}" unless $reported;
    ok $reported, 'a file created inside it is reported by the nested watcher';
    # TEMPORARY DIAGNOSTICS (CI shows only failed test numbers): 6 = the file exists,
    # 7 = a fresh plain watcher on the dir sees a file created now, 8 = the root
    # watcher's own events included 'svc'.
    ok $dir.add('svc/.cro.yml').e, 'diag6: file exists';
    my $fresh = Channel.new;
    my $ftap = $dir.add('svc').watch.tap: -> $e { $fresh.send($e.path.IO.basename) };
    sleep 0.5;
    spurt $dir.add('svc/diag7.txt'), "y\n";
    my @fs; my $fd = now + 5;
    while now < $fd { if $fresh.poll -> $n { @fs.push($n) } else { sleep 0.05 }; last if @fs.grep('diag7.txt') }
    $ftap.close;
    ok @fs.grep('diag7.txt').elems > 0, 'diag7: a fresh watcher sees a new file';
    ok @seen.grep('svc').elems > 0, 'diag8: svc was reported';
    my @dg; while $diag.poll -> $m { @dg.push($m) }
    ok @dg.grep('add').elems >= 2, 'diag9: add-dir ran for the new directory';
    nok @dg.grep('caught').elems, 'diag10: nothing was thrown in the whenever body';
    my $m = @dg.first(*.starts-with('msg:')) // '';
    # message probe: each candidate is a separate numbered assertion
    for ('No such method', 'Undeclared', 'Cannot', 'Too', 'IO', 'path', 'Type check', 'not found', 'Unknown', 'X::AdHoc', 'X::Method::NotFound', 'X::IO', 'X::TypeCheck') -> $needle {
        ok !$m.contains($needle), "diag-msg: no '$needle'";
    }
    ok @dg.grep('isdir').elems >= 1, 'diag11: the event path is a directory';
    ok @dg.grep('body').elems >= 2, 'diag12: whenever bodies ran at least twice (root + nested)';
}

# An `emit` outside every supply still raises, as before.
{
    sub bare-emit { emit 1 }
    throws-like { bare-emit() }, X::ControlFlow, 'emit without a supply is still refused';
}
