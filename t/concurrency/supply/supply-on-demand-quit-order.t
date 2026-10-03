use Test;

# A `Supply.on-demand` producer that quits synchronously delivers the values
# it emitted before the quit first, then the quit (#11237).

plan 6;

{
    my @log;
    Supply.on-demand(-> $p { $p.emit(1); $p.emit(2); $p.quit("boom") }).tap(
        { @log.push: "v$_" },
        quit => { @log.push: "Q " ~ .message },
    );
    is-deeply @log, ["v1", "v2", "Q boom"], 'values before the quit reach the tap first';
}

{
    my @log;
    Supply.on-demand(-> $p { $p.emit(1); $p.quit("boom"); $p.emit(2) }).tap(
        { @log.push: "v$_" },
        quit => { @log.push: "Q " ~ .message },
        done => { @log.push: "D" },
    );
    is-deeply @log, ["v1", "Q boom"], 'an emit after the quit is dropped and done never fires';
}

{
    my @log;
    Supply.on-demand(-> $p { $p.emit(1); $p.quit("boom"); @log.push: "after" }).tap(
        { @log.push: "v$_" },
        quit => { @log.push: "Q " ~ .message },
    );
    is-deeply @log, ["v1", "Q boom", "after"], 'a tap sees the quit when it happens, inside the producer';
}

{
    my @log;
    react {
        whenever Supply.on-demand(-> $p { $p.emit(1); $p.quit("boom") }) {
            @log.push: "v$_";
            QUIT { default { @log.push: "QUIT " ~ .message } }
        }
    }
    is-deeply @log, ["v1", "QUIT boom"], 'a whenever sees the value before its QUIT';
}

{
    my $s = Supply.on-demand(-> $p { $p.emit(1); $p.quit("boom") });
    throws-like { await $s }, Exception, message => 'boom', 'await rethrows the quit reason';
}

{
    my @log;
    supply { emit 1; die "boom" }.tap({ @log.push: "v$_" }, quit => { @log.push: "Q " ~ .message });
    is-deeply @log, ["v1", "Q boom"], 'a supply block that dies keeps the same order';
}
