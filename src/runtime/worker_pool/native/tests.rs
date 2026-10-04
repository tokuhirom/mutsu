//! Unit tests for the worker pool's growth rule (`PoolState::growth`).

use super::*;

fn state(queue: usize, idle: usize, live: usize, starting: usize, blocked: usize) -> PoolState {
    let mut q = VecDeque::new();
    for _ in 0..queue {
        q.push_back(Task {
            run: Box::new(|| {}),
            reject: None,
        });
    }
    PoolState {
        queue: q,
        idle,
        live,
        starting,
        blocked,
    }
}

#[test]
fn no_unclaimed_work_means_no_growth() {
    // One queued task, one idle worker about to take it.
    assert_eq!(state(1, 1, 1, 0, 0).growth(8), None);
    // One queued task, one worker starting for it.
    assert_eq!(state(1, 0, 1, 1, 0).growth(8), None);
}

#[test]
fn grows_within_the_soft_cap() {
    assert_eq!(state(1, 0, 2, 0, 0).growth(8), Some(StackPolicy::Budgeted));
}

#[test]
fn queues_past_the_soft_cap_while_someone_runs() {
    assert_eq!(state(5, 0, 8, 0, 0).growth(8), None);
    // Blocked workers do not count toward the cap.
    assert_eq!(state(5, 0, 8, 0, 3).growth(8), Some(StackPolicy::Budgeted));
}

#[test]
fn all_blocked_forces_growth() {
    assert_eq!(state(1, 0, 8, 0, 8).growth(8), Some(StackPolicy::Required));
    // Nobody at all (first submit).
    assert_eq!(state(1, 0, 0, 0, 0).growth(8), Some(StackPolicy::Required));
    // A starting worker is spoken for by another task.
    assert_eq!(state(2, 0, 3, 1, 2).growth(8), Some(StackPolicy::Required));
}
