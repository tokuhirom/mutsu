use Test;
plan 3;

# todo/tickets/procasync-stdout-is-not-incremental.md: a tap registered
# before `.start()` must receive chunks as the child writes them, not just
# once as a single blob when the child exits.
{
    my $p = Proc::Async.new(
        $*EXECUTABLE.absolute, '-e',
        q{$*OUT.print("EARLY\n"); $*OUT.flush; sleep 1; $*OUT.print("LATE\n");},
    );
    my @chunks;
    my @times;
    my $t0 = now;
    $p.stdout.tap(-> $c { @chunks.push($c); @times.push(now - $t0) });
    await $p.start;

    # Not `== 2`: the decoder holds each chunk's final grapheme back until the
    # next read proves nothing extends it (see
    # `news/2026-09/procasync-holds-back-the-final-grapheme.md`), so EARLY's
    # trailing "\n" rides along with LATE and the very last "\n" is flushed on
    # its own. What this file is about is *when* the chunks arrive, not where
    # they are cut.
    is @chunks.join(''), "EARLY\nLATE\n", 'the whole output arrived';
    ok @times[1] - @times[0] > 0.5,
        'the second chunk arrived measurably later than the first (streamed, not batched)';
}

# A tap registered before `.start()` inside a `react`/`whenever` block must
# still see the full accumulated output once the process exits — the live
# streaming path is skipped there (see native_proc_async.rs's `.start()`
# handling) because a `whenever` body shares lexicals with its siblings
# through the react loop's own dispatch, not a general cross-thread cell.
{
    my $out = '';
    react {
        my $p = Proc::Async.new($*EXECUTABLE.absolute, '-e', 'say "hi"');
        whenever $p.stdout {
            $out ~= $_;
        }
        whenever $p.start {
            done;
        }
    }
    is $out, "hi\n", 'react/whenever tap still sees the full output after the process exits';
}
