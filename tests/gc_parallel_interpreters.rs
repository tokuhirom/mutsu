//! #11714: interpreters running on parallel threads of one process must not
//! corrupt each other's GC bookkeeping.
//!
//! The cycle collector's candidate buffer is process-global, and trial
//! deletion is only sound while every other mutator is stopped. A thread that
//! ran its own `Interpreter` (an embedder, or `cargo test` itself) used to be
//! invisible to the stop-the-world accounting, so a collect on one such thread
//! skipped the stop and trial-deleted nodes another thread was still dropping.
//! In a debug build that aborted the process with `Gc::drop strong-count
//! underflow` (first seen in the `*_intern_budget` tests).
//!
//! Each thread here builds cyclic garbage so the collector has suspects to
//! scan, and keeps an instance alive in an `END` phaser's environment, which
//! is the drop the original backtrace failed in. The race is timing-dependent,
//! so this test exercises the shape rather than proving its absence; the
//! deterministic pin on the registration itself is the unit test
//! `an_interpreter_thread_registers_until_it_exits` in `src/gc/stw.rs`.

const PROGRAM: &str = r#"
class Node { has $.next is rw; has $.payload }
for ^1500 {
    my $a = Node.new(payload => $_);
    my $b = Node.new(next => $a);
    $a.next = $b;
}
my $held = Node.new(payload => 'end');
END { $held.payload }
say 'done';
"#;

fn run_rounds(rounds: usize) {
    for _ in 0..rounds {
        let mut interp = mutsu::Interpreter::new();
        let out = interp.run(PROGRAM).expect("program runs");
        assert!(out.contains("done"), "unexpected output: {out:?}");
        drop(interp);
    }
}

#[test]
fn interpreters_on_parallel_threads_share_the_collector_soundly() {
    // A debug-build parse needs more than libtest's 2 MiB default stack.
    const STACK_SIZE: usize = 64 * 1024 * 1024;
    let handles: Vec<_> = (0..4)
        .map(|i| {
            std::thread::Builder::new()
                .name(format!("gc-parallel-{i}"))
                .stack_size(STACK_SIZE)
                .spawn(|| run_rounds(3))
                .expect("spawn interpreter thread")
        })
        .collect();
    for handle in handles {
        handle.join().expect("interpreter thread");
    }
}
