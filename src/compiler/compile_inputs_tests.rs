use super::*;

#[test]
fn a_recording_logs_questions_and_revalidates_them() {
    let guard = start().expect("no recording open yet");
    assert!(
        start().is_none(),
        "a nested compile shares the open recording"
    );
    let _ = current_language_version_starts_with("6.");
    let _ = is_user_declared_type("NoSuchTypeAnywhere");
    let Recorded::Cacheable(inputs) = guard.finish() else {
        panic!("nothing marked the compile uncacheable");
    };
    assert_eq!(inputs.len(), 2);
    assert!(inputs.still_hold());
}

#[test]
fn an_unvalidatable_read_makes_the_compile_uncacheable() {
    let guard = start().expect("no recording open yet");
    mark_uncacheable("test");
    assert!(matches!(guard.finish(), Recorded::Uncacheable("test")));
}

#[test]
fn outside_a_recording_the_wrappers_record_nothing() {
    let _ = is_imported_function("whatever");
    let guard = start().expect("no recording open yet");
    let Recorded::Cacheable(inputs) = guard.finish() else {
        panic!("empty recording is cacheable");
    };
    assert_eq!(inputs.len(), 0);
}
