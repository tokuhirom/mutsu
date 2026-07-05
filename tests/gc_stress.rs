//! Integration coverage for the GC cycle collector running *during* real
//! execution (safepoint wiring, §11 step 8 second half). The collector's
//! algorithm is unit-tested in `src/gc/collect.rs`; these tests exercise it
//! end-to-end through the built interpreter under the `MUTSU_GC` stress modes.
//!
//! Default runs (no `MUTSU_GC`) never collect, so this is the only place the
//! collector meets real interpreter state — the key property being that
//! collecting aggressively must not disturb *live* data.

use std::process::Command;

/// Run a Raku snippet through the built `mutsu`, optionally with a GC stress
/// mode enabled. Returns (stdout, stderr, success).
fn run(src: &str, gc: &[(&str, &str)]) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    // Start from a clean GC configuration regardless of the ambient environment
    // (the `gc-stress` CI job runs `cargo test` with `MUTSU_GC=on`), so each test
    // controls exactly the mode it exercises.
    for k in [
        "MUTSU_GC",
        "MUTSU_GC_EVERY_SAFEPOINT",
        "MUTSU_GC_EVERY_CANDIDATE",
        "MUTSU_GC_AT",
        "MUTSU_GC_COLLECT_NOW",
        "MUTSU_GC_RANDOM_RATE",
        "MUTSU_GC_RANDOM_SEED",
        "MUTSU_GC_THRESHOLD",
        "MUTSU_GC_VERIFY",
        "MUTSU_GC_LOG",
    ] {
        cmd.env_remove(k);
    }
    // GC is default-ON since the ADR-0003 flip, so the no-vars baseline must
    // opt out explicitly — these tests compare "GC off" against the exact
    // stress mode they set (which overrides this below).
    cmd.env("MUTSU_GC", "off");
    for (k, v) in gc {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

/// Collecting on every instruction must produce byte-identical output to not
/// collecting at all — i.e. the collector never reclaims reachable/live data.
#[test]
fn collect_every_instruction_preserves_live_data() {
    let src = r#"
        my %counts;
        my @data = (1..60).map({ $_ * 3 });
        for @data -> $n { my $k = ($n % 5).Str; %counts{$k} = (%counts{$k} // 0) + $n; }
        say %counts.sort.map({ .key ~ "=" ~ .value }).join(",");
        my %nested = a => { b => [1,2,3] }, c => { d => [4,5,6] };
        say %nested<a><b>.sum + %nested<c><d>.sum;
        say @data.grep(* > 100).sum;
    "#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, on_err, ok_on) = run(
        src,
        &[("MUTSU_GC", "on"), ("MUTSU_GC_EVERY_SAFEPOINT", "1")],
    );

    assert!(ok_off, "GC-off run must succeed");
    assert!(ok_on, "GC-stress run must not crash; stderr:\n{on_err}");
    assert_eq!(
        off, on,
        "GC collecting every instruction changed program output"
    );
}

/// A `Gc` container reachable only through a non-node wrapper (`Pair`, itemized
/// `Scalar`, ...) must still be traced — i.e. cycles *through* those wrappers
/// get reclaimed, and the extra traced edges never over-collect live data.
#[test]
fn wrapper_nested_cycles_are_reclaimed_and_preserve_data() {
    // Correctness: a Pair/wrapper-heavy computation is identical GC-on vs off.
    let compute = r#"
        my %m;
        my @pairs;
        for 1..25 -> $i { @pairs.push( $i => [$i, $i * $i] ); }
        for @pairs -> $p { %m{$p.key} = $p.value.sum; }
        say %m.keys.map(*.Int).sort.map({ $_ ~ ":" ~ %m{$_} }).join("|");
        say @pairs.map({ .value.sum }).sum;
    "#;
    let (off, _, ok_off) = run(compute, &[]);
    let (on, on_err, ok_on) = run(
        compute,
        &[("MUTSU_GC", "on"), ("MUTSU_GC_EVERY_SAFEPOINT", "1")],
    );
    assert!(ok_off && ok_on, "runs must succeed; stderr:\n{on_err}");
    assert_eq!(off, on, "wrapper-heavy compute output diverged under GC");

    // Reclaim: a cycle whose back-edge passes through a `Pair` must be collected.
    let cyclic = r#"
        for ^25 { my %h; %h<p> = ("k" => %h); }
        say "done";
    "#;
    let (out, err, ok) = run(
        cyclic,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );
    assert!(
        ok && out.contains("done"),
        "cyclic run failed; stderr:\n{err}"
    );
    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|t| t.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "cycle through a Pair wrapper must be reclaimed; reclaimed_nodes={reclaimed}\n{err}"
    );
}

/// `MUTSU_GC_VERIFY=1` runs the collector's soundness/heap-sanity checks around
/// every collect; a correct collector must report zero violations on real
/// programs, cyclic or not.
#[test]
fn verify_mode_reports_no_violations() {
    let programs = [
        // Heavy cycle churn.
        r#"for ^40 { my %h; %h<self> := %h; my %x; my %y; %x<y> := %y; %y<x> := %x; }; say "ok";"#,
        // Compute with live nested structures (nothing should be reclaimed wrongly).
        r#"my %m; for 1..30 -> $i { %m{$i} = [$i, $i*$i].sum; }; say %m.values.sum;"#,
    ];
    for src in programs {
        let (out, err, ok) = run(
            src,
            &[
                ("MUTSU_GC", "on"),
                ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
                ("MUTSU_GC_VERIFY", "1"),
            ],
        );
        assert!(ok, "verify run must not crash; stderr:\n{err}");
        assert!(!out.is_empty(), "program produced output");
        assert!(
            !err.contains("VERIFY FAIL"),
            "collector verify reported a violation:\n{err}"
        );
    }
}

/// `MUTSU_GC_LOG=summary` emits a start/end line per non-empty collect, and
/// `trace` emits a per-reclaimed-node line — the debug-log surface (§9.4).
#[test]
fn log_modes_emit_expected_lines() {
    let cyclic = r#"for ^10 { my %h; %h<self> := %h; }; say "ok";"#;

    let (_, summary_err, ok1) = run(
        cyclic,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_GC_LOG", "summary"),
        ],
    );
    assert!(ok1);
    assert!(
        summary_err.contains("[mutsu gc] start cycle=")
            && summary_err.contains("[mutsu gc] end cycle="),
        "summary log missing start/end lines:\n{summary_err}"
    );
    assert!(
        summary_err.contains("reason=backedge"),
        "summary log missing the safepoint reason:\n{summary_err}"
    );

    let (_, trace_err, ok2) = run(
        cyclic,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_GC_LOG", "trace"),
        ],
    );
    assert!(ok2);
    // A garbage node is reclaimed either by the cycle scan (`reclaim cycle=`)
    // or, when its refcount already hit 0 in the buffer, by the dead sweep
    // (`reclaim dead`) — trace mode logs a per-node line in both cases.
    assert!(
        trace_err.contains("[mutsu gc] reclaim cycle=")
            || trace_err.contains("[mutsu gc] reclaim dead"),
        "trace log missing per-node reclaim lines:\n{trace_err}"
    );
}

/// Mutual cycles through user-class `is rw` attributes — the motivating case for
/// the object-graph collector (§11 / `WeakGc` layer 3a). Two `Instance`s point at
/// each other through attribute containers; dropping both must reclaim the cycle,
/// while a *live* object cycle held by a lexical must survive an aggressive collect
/// and stay traversable (`.next.name` round-trips).
#[test]
fn object_attribute_cycles_reclaimed_and_live_ones_survive() {
    let src = r#"
        class Node { has $.name is rw; has $.next is rw; }
        # A live cycle, reachable through $keeper-a/$keeper-b for the whole run.
        my $keeper-a = Node.new(name => "A");
        my $keeper-b = Node.new(name => "B");
        $keeper-a.next = $keeper-b;
        $keeper-b.next = $keeper-a;
        # Dropped mutual cycles: collected mid-run under the safepoint stress mode.
        for ^30 {
            my $x = Node.new(name => "x");
            my $y = Node.new(name => "y");
            $x.next = $y;
            $y.next = $x;
        }
        say $keeper-a.next.name;        # B
        say $keeper-b.next.name;        # A
        say $keeper-a.next.next.name;   # A (round-trip proves the live cycle intact)
        say "done";
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_GC_VERIFY", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "object-cycle run must not crash; stderr:\n{err}");
    assert_eq!(
        out, "B\nA\nA\ndone\n",
        "live object cycle was corrupted or over-collected; stderr:\n{err}"
    );
    assert!(
        !err.contains("VERIFY FAIL"),
        "collector verify reported a violation:\n{err}"
    );

    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "mutual object-attribute cycles must be reclaimed; reclaimed_nodes={reclaimed}\nstderr:\n{err}"
    );
}

/// A program that repeatedly builds self-referential / mutual cycles and drops
/// them must actually get those cycles reclaimed (not just leak), and survive.
#[test]
fn cycles_are_reclaimed_during_execution() {
    let src = r#"
        for ^30 {
            my %h; %h<self> := %h;
            my %x; my %y; %x<y> := %y; %y<x> := %x;
        }
        say "done";
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "cycle-stress run must not crash; stderr:\n{err}");
    assert!(out.contains("done"), "program must run to completion");

    // The `[mutsu vm-stats] gc: ... reclaimed_nodes=N ...` summary must report a
    // positive reclaim — proof the collector broke the cycles.
    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "expected the collector to reclaim cycle nodes, got reclaimed_nodes={reclaimed}\nstderr:\n{err}"
    );
}

/// Raku `DESTROY` must fire under GC=on exactly as it does GC-off. The
/// candidate buffer's retained handle defers the value's Rust `Drop`
/// indefinitely, so DESTROY queueing is decoupled from `Drop` into
/// `Trace::finalize`, which runs at last-live-handle drop (refcount death) and
/// at cycle reclaim. Regression: t/destroy.t returned `got: []` under
/// `MUTSU_GC=on` before the finalize hook existed.
#[test]
fn destroy_fires_on_refcount_death_under_gc() {
    let src = r#"
        my @events;
        class Foo { submethod DESTROY { @events.push("foo") } }
        my $foo = Foo.new;
        $foo = Nil;
        quietly $*VM.request-garbage-collection;
        say @events.join(",");
    "#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, on_err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_CANDIDATE", "64"),
            ("MUTSU_GC_VERIFY", "1"),
        ],
    );

    assert!(ok_off, "GC-off run must succeed");
    assert!(ok_on, "GC-on run must not crash; stderr:\n{on_err}");
    assert_eq!(off.trim(), "foo", "GC-off DESTROY baseline");
    assert_eq!(on.trim(), "foo", "GC-on DESTROY must match GC-off");
    assert!(
        !on_err.contains("VERIFY FAIL"),
        "no soundness violations; stderr:\n{on_err}"
    );
}

/// An object kept alive ONLY by a reference cycle gets its DESTROY when the
/// collector reclaims the cycle — the finalize pass runs before
/// `drop_gc_edges` clears the attributes, so the submethod still sees them.
#[test]
fn destroy_fires_on_cycle_reclaim() {
    let src = r#"
        my @events;
        class Node {
            has $.name;
            has $.peer is rw;
            submethod DESTROY { @events.push($!name) }
        }
        sub make-cycle() {
            my $a = Node.new(name => "a");
            my $b = Node.new(name => "b");
            $a.peer = $b;
            $b.peer = $a;
        }
        make-cycle();
        quietly $*VM.request-garbage-collection;
        say @events.sort.join(",");
    "#;

    let (on, on_err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_SAFEPOINT", "1"),
            ("MUTSU_GC_VERIFY", "1"),
        ],
    );

    assert!(ok_on, "GC-on run must not crash; stderr:\n{on_err}");
    assert_eq!(
        on.trim(),
        "a,b",
        "both cycle members' DESTROY must fire at reclaim; stderr:\n{on_err}"
    );
    assert!(
        !on_err.contains("VERIFY FAIL"),
        "no soundness violations; stderr:\n{on_err}"
    );
}

/// While worker threads are live the cycle scan defers (no cross-thread STW),
/// but the dead sweep must still run at safepoints and bound the candidate
/// buffer: a threaded mutation-heavy loop must not defer every container
/// release to one giant post-join collect. Guards both correctness (all cas
/// increments land) and the pause bound (no multi-second single collect).
#[test]
fn dead_sweep_bounds_threaded_mutation_memory() {
    let src = r#"
        my %seen;
        my $times = 400;
        %seen{^$times} = (0 xx $times);
        my @t = (1..2).map: { Thread.start({
            cas %seen{$_}, {.succ} for ^$times;
        }) };
        .finish for @t;
        say "sum=", [+] %seen.values;
    "#;

    let (on, on_err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_CANDIDATE", "64"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(
        ok_on,
        "threaded GC-on run must not crash; stderr:\n{on_err}"
    );
    assert_eq!(on.trim(), "sum=800", "every cas increment must land");

    // The dead sweep runs during the loop, so the maximum single pause must
    // stay far below the giant post-join collect it replaced (which scanned
    // every dead snapshot: seconds). 500ms is a generous CI-load bound.
    let pause_max_ns = on_err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("pause_ns_max="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(u64::MAX);
    assert!(
        pause_max_ns < 500_000_000,
        "expected bounded collect pauses with the dead sweep, got pause_ns_max={pause_max_ns}\nstderr:\n{on_err}"
    );
}

/// `MUTSU_GC_AT=call,return` collects only at the call/return safepoint kinds
/// (§9.2): the run must stay correct, actually reclaim the per-call cycles, and
/// every logged collect reason must be one of the listed kinds.
#[test]
fn at_kind_filter_collects_only_at_listed_safepoints() {
    let src = r#"
        sub leak-cycle($n) { my %h; %h<self> := %h; %h<n> = $n; $n * 2 }
        my $sum = 0;
        for ^30 { $sum += leak-cycle($_) }
        say $sum;
    "#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_AT", "call,return"),
            ("MUTSU_GC_LOG", "summary"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok_off && ok_on, "runs must succeed; stderr:\n{err}");
    assert_eq!(off, on, "MUTSU_GC_AT run changed program output");
    let reasons: Vec<&str> = err
        .lines()
        .filter(|l| l.contains("[mutsu gc] start cycle="))
        .filter_map(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reason="))
        })
        .collect();
    assert!(
        !reasons.is_empty(),
        "expected at least one collect at a call/return safepoint:\n{err}"
    );
    // `program-end` is the unconditional final collect (`collect_if_enabled`),
    // not a safepoint fire — only the *safepoint* reasons must obey the filter.
    assert!(
        reasons
            .iter()
            .all(|r| *r == "call" || *r == "return" || *r == "program-end"),
        "collect fired at a kind outside MUTSU_GC_AT=call,return: {reasons:?}\n{err}"
    );
    assert!(
        reasons.iter().any(|r| *r == "call" || *r == "return"),
        "expected at least one call/return-safepoint collect: {reasons:?}\n{err}"
    );
    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "per-call cycles must be reclaimed under MUTSU_GC_AT; stderr:\n{err}"
    );
}

/// Seeded random stress (§9.2): `MUTSU_GC_RANDOM_RATE` collects probabilistically
/// but deterministically for a fixed `MUTSU_GC_RANDOM_SEED` — output must match
/// GC-off, and the seed must be logged so a failing run can be replayed.
#[test]
fn random_stress_is_seeded_and_preserves_output() {
    let src = r#"
        my %m;
        for 1..40 -> $i { my %h; %h<self> := %h; %m{$i % 7} = (%m{$i % 7} // 0) + $i; }
        say %m.sort.map({ .key ~ "=" ~ .value }).join(",");
    "#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_RANDOM_RATE", "0.5"),
            ("MUTSU_GC_RANDOM_SEED", "42"),
            ("MUTSU_GC_VERIFY", "1"),
        ],
    );

    assert!(ok_off && ok_on, "runs must succeed; stderr:\n{err}");
    assert_eq!(off, on, "random-stress run changed program output");
    assert!(
        err.contains("random stress: rate=0.5 seed=42"),
        "the random seed must be logged for replay:\n{err}"
    );
    assert!(
        !err.contains("VERIFY FAIL"),
        "no soundness violations under random stress:\n{err}"
    );
}

/// `MUTSU_GC_COLLECT_NOW=1` (§9.2): one collect fires at program start (before
/// any user code) and the program still runs normally.
#[test]
fn collect_now_runs_one_startup_collect() {
    let src = r#"say "started";"#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_COLLECT_NOW", "1"),
            ("MUTSU_GC_LOG", "summary"),
        ],
    );

    assert!(ok, "collect-now run must succeed; stderr:\n{err}");
    assert_eq!(out.trim(), "started");
    // A startup collect on an empty heap is a silent no-op, so the trigger
    // logs its own firing under MUTSU_GC_LOG.
    assert!(
        err.contains("[mutsu gc] collect-now at startup"),
        "expected the startup collect trigger to fire; stderr:\n{err}"
    );
}

/// Worker threads churning reference CYCLES while other workers run: the
/// cooperative stop-the-world must let the cycle scan reclaim garbage without
/// ever corrupting live data (VERIFY clean, correct output). This is the
/// gc::stw end-to-end exercise — before it, the scan simply deferred until
/// every worker joined.
#[test]
fn threaded_cycle_churn_is_collected_soundly() {
    let src = r#"
        my @sums;
        await start {
            my $local-sum = 0;
            for ^60 {
                # A garbage self-cycle per iteration...
                my %h; %h<self> := %h; %h<n> = $_;
                # ...plus live data the collector must not disturb.
                $local-sum += $_;
            }
            $local-sum;
        } xx 3;
        say "done";
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_CANDIDATE", "16"),
            ("MUTSU_GC_VERIFY", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "threaded cycle churn must not crash; stderr:\n{err}");
    assert!(out.contains("done"), "program must run to completion");
    assert!(
        !err.contains("VERIFY FAIL"),
        "no soundness violations under threaded cycle churn:\n{err}"
    );
    // The cycles must be reclaimed by program end (mid-run via STW scans
    // and/or at the final single-threaded collect).
    let reclaimed = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("reclaimed_nodes="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "cycle garbage from worker threads must be reclaimed; stderr:\n{err}"
    );
}

/// Rapid short-lived worker churn (a `Promise.start` per loop iteration, RT
/// #128628 shape — S17-lowlevel/semaphore.t) must not starve the cooperative
/// stop-the-world. Two regressions made this degrade from seconds to a CI
/// timeout under GC=on, each burning ~the full 50ms STW wait at nearly every
/// collect: (1) the spawn *birth window* — a worker counted in the quiescence
/// target from `enter_mutator_worker` (parent-side) but unable to park until
/// it starts running — fixed by parent-side `preregister_worker_quiescent`;
/// (2) a worker *exit* satisfies the quiescence equation by lowering the
/// target, which never woke the waiting collector — fixed by
/// `notify_worker_exit`. Guard both via the accumulated pause total: with
/// either regression, ~50ms × (collects during 300 spawns) blows far past the
/// bound (measured pre-fix: multiple seconds; post-fix: tens of ms).
#[test]
fn spawn_churn_does_not_starve_stop_the_world() {
    let src = r#"
        my Semaphore $s .= new(1);
        my @p;
        my $r = 0;
        for ^300 {
            my $i = $_;
            @p.push: Promise.start({ $s.acquire; $r += $i; $s.release; });
        }
        await @p;
        say $r;
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_EVERY_CANDIDATE", "64"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "spawn-churn run must not crash; stderr:\n{err}");
    assert_eq!(out.trim(), "44850", "semaphore-protected sum must be exact");

    let pause_total_ns = err
        .lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix("pause_ns_total="))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(u64::MAX);
    assert!(
        pause_total_ns < 2_000_000_000,
        "STW must not repeatedly time out under spawn churn; pause_ns_total={pause_total_ns}\nstderr:\n{err}"
    );
}

/// Pull the `key=N` counter out of the `[mutsu vm-stats] gc:` summary line.
fn gc_stat(err: &str, key: &str) -> u64 {
    let prefix = format!("{key}=");
    err.lines()
        .find(|l| l.contains("[mutsu vm-stats] gc:"))
        .and_then(|l| {
            l.split_whitespace()
                .find_map(|tok| tok.strip_prefix(prefix.as_str()))
        })
        .and_then(|n| n.parse::<u64>().ok())
        .unwrap_or(u64::MAX)
}

/// ADR-0003 production trigger: with ONLY `MUTSU_GC=on` (no stress vars), the
/// candidate-buffer size threshold must fire collects mid-run — garbage
/// cycles get reclaimed before program end, and the output is identical to
/// GC-off. (Pre-ADR-0003, plain GC=on meant program-end collect only.)
#[test]
fn size_threshold_trigger_collects_without_stress_vars() {
    let src = r#"for ^400 { my %h; %h<self> := %h; }; say "done";"#;

    let (off, _, ok_off) = run(src, &[]);
    let (on, err, ok_on) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_THRESHOLD", "256"),
            ("MUTSU_GC_VERIFY", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok_off && ok_on, "runs must succeed; stderr:\n{err}");
    assert_eq!(off, on, "size-threshold run changed program output");
    assert!(
        !err.contains("VERIFY FAIL"),
        "no soundness violations under the size trigger:\n{err}"
    );
    // Mid-run collects (threshold crossings) plus the program-end one.
    let collections = gc_stat(&err, "collections");
    assert!(
        collections >= 2,
        "size threshold must fire before program end; collections={collections}\n{err}"
    );
    assert!(
        gc_stat(&err, "reclaimed_nodes") > 0,
        "cycle garbage must be reclaimed; stderr:\n{err}"
    );
}

/// ADR-0003 adaptive backoff: a workload whose candidates are mostly a GROWING
/// LIVE structure (the cas-loop.t shape — every scan re-proves liveness) must
/// raise the effective threshold (`2 × survivors`) instead of re-scanning the
/// live graph on a fixed period, keeping the collect count logarithmic-ish
/// rather than pushes/BASE.
#[test]
fn adaptive_threshold_backs_off_on_live_suspects() {
    let src = r#"
        class Node { has $.v; has $.next }
        my $head = Node;
        for ^800 { $head = Node.new(v => $_, next => $head); }
        say $head.v;
    "#;

    let (out, err, ok) = run(
        src,
        &[
            ("MUTSU_GC", "on"),
            ("MUTSU_GC_THRESHOLD", "64"),
            ("MUTSU_GC_VERIFY", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );

    assert!(ok, "live-chain run must succeed; stderr:\n{err}");
    assert_eq!(out.trim(), "799", "live chain must stay intact");
    assert!(
        !err.contains("VERIFY FAIL"),
        "no soundness violations on a live suspect graph:\n{err}"
    );
    let threshold = gc_stat(&err, "gc_threshold");
    assert!(
        threshold > 64,
        "unproductive scans must raise the threshold (2 x survivors); gc_threshold={threshold}\n{err}"
    );
    // ~830 candidate pushes at BASE=64 would be ~13 fixed-period scans of the
    // growing chain; the geometric backoff needs only a handful (4 measured).
    let collections = gc_stat(&err, "collections");
    assert!(
        collections <= 8,
        "adaptive backoff must bound rescans of a growing live graph; collections={collections}\n{err}"
    );
}
