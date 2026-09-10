use Test;

plan 1;

{
    my constant RUNS = 6;
    my $watch-file = $*SPEC.catfile($*TMPDIR, "mutsu-io-watch");
    my $fh = $watch-file.IO.open: :w, :0out-buffer;
    my $start-writing = Promise.new;
    my $start-vow = $start-writing.vow;
    my @proceed = Promise.new xx RUNS;
    @proceed[0].keep;

    start {
        await $start-writing;
        for ^RUNS {
            await @proceed[$_];
            $fh.say: $_;
            $fh.flush;
        }
    }

    my $count = 0;
    my $timeout = Promise.in(5);
    react {
        whenever $watch-file.IO.watch -> $e {
            $count++;
            @proceed[$count].?keep;
            done if $count == RUNS;
        }
        whenever $timeout {
            done;
        }
        whenever Promise.in(0.3) {
            $start-vow.keep(True);
        }
    }

    if $timeout.status ~~ Planned {
        is $count, RUNS, "IO.watch emits each file change";
    }
    else {
        flunk "timed out waiting for watch events";
    }

    LEAVE {
        $fh.close;
        $watch-file.IO.unlink;
    }
}
