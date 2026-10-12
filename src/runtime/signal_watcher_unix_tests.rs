use super::*;
use crate::runtime::native_methods::supply_channel::supply_event_channel;

/// A slot built without touching the process's dispositions: the
/// zeroed `previous_action` is `SIG_DFL` with no flags, which is what
/// an unwatched signal already has.
fn slot(registrations: Vec<SignalRegistration>) -> SignalSlot {
    SignalSlot {
        registrations,
        previous_action: unsafe { std::mem::zeroed() },
    }
}

#[test]
fn the_sweep_drops_retired_registrations_and_keeps_live_ones() {
    let (live_tx, live_template) = supply_event_channel();
    let _live_tap = live_template.subscribe();
    let (dead_tx, dead_template) = supply_event_channel();
    drop(dead_template.subscribe());

    let mut registry = std::collections::HashMap::new();
    registry.insert(
        libc::SIGUSR2,
        slot(vec![
            SignalRegistration::Supply {
                supply_id: 1,
                tx: live_tx,
                value: Value::int(1),
            },
            SignalRegistration::Supply {
                supply_id: 2,
                tx: dead_tx,
                value: Value::int(2),
            },
        ]),
    );

    let mut orphaned = Vec::new();
    sweep_locked(&mut registry, &mut orphaned);

    assert_eq!(orphaned, vec![2]);
    let slot = registry.get(&libc::SIGUSR2).expect("slot kept");
    assert_eq!(slot.registrations.len(), 1);
    assert_eq!(slot.registrations[0].supply_id(), Some(1));
}

#[test]
fn a_signal_that_loses_its_last_registration_leaves_the_registry() {
    let (tx, template) = supply_event_channel();
    drop(template.subscribe());

    let mut registry = std::collections::HashMap::new();
    registry.insert(
        libc::SIGUSR2,
        slot(vec![SignalRegistration::Supply {
            supply_id: 7,
            tx,
            value: Value::int(7),
        }]),
    );

    let mut orphaned = Vec::new();
    sweep_locked(&mut registry, &mut orphaned);

    assert!(registry.is_empty());
    assert_eq!(orphaned, vec![7]);
}

#[test]
fn an_untapped_supply_is_left_alone() {
    // `signal(SIGTERM)` that nobody has tapped *yet* must keep its
    // registration: its first tap may still be coming.
    let (tx, _template) = supply_event_channel();

    let mut registry = std::collections::HashMap::new();
    registry.insert(
        libc::SIGUSR2,
        slot(vec![SignalRegistration::Supply {
            supply_id: 3,
            tx,
            value: Value::int(3),
        }]),
    );

    let mut orphaned = Vec::new();
    sweep_locked(&mut registry, &mut orphaned);

    assert!(orphaned.is_empty());
    assert_eq!(registry[&libc::SIGUSR2].registrations.len(), 1);
}

#[test]
fn a_real_registration_is_gone_once_its_last_tap_is() {
    // End to end over the process-global registry, including the
    // sigaction install and restore: `roast/S17-procasync/stress.t`
    // runs 1200 of these cycles and every one of them used to stay.
    let (tx, template) = supply_event_channel();
    let tap = template.subscribe();
    register_watcher(
        libc::SIGUSR2,
        SignalRegistration::Supply {
            supply_id: 4242,
            tx,
            value: Value::int(0),
        },
    );
    assert_eq!(live_registrations(libc::SIGUSR2), 1);

    drop(tap);
    drop(template);
    sweep_retired_registrations();
    assert_eq!(live_registrations(libc::SIGUSR2), 0);
}

#[test]
fn a_multi_signal_supply_is_orphaned_only_once_every_signum_is_gone() {
    // `signal(SIGINT, SIGTERM)` registers the same supply id twice;
    // discarding its channel while the other registration still lives
    // would cut off a tap that is still running.
    let (retired_tx, retired_template) = supply_event_channel();
    drop(retired_template.subscribe());
    let (live_tx, live_template) = supply_event_channel();
    let _live_tap = live_template.subscribe();

    let mut registry = std::collections::HashMap::new();
    registry.insert(
        libc::SIGUSR1,
        slot(vec![SignalRegistration::Supply {
            supply_id: 9,
            tx: retired_tx,
            value: Value::int(9),
        }]),
    );
    registry.insert(
        libc::SIGUSR2,
        slot(vec![SignalRegistration::Supply {
            supply_id: 9,
            tx: live_tx,
            value: Value::int(9),
        }]),
    );

    let mut orphaned = Vec::new();
    sweep_locked(&mut registry, &mut orphaned);

    assert!(orphaned.is_empty(), "channel still serves the live signal");
    assert!(!registry.contains_key(&libc::SIGUSR1));
    assert!(registry.contains_key(&libc::SIGUSR2));
}
