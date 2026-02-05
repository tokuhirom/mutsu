use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_plan_skip_all_subtests() {
    let program = fs::read_to_string("../roast/S24-testing/11-plan-skip-all-subtests.t")
        .expect("read roast plan skip-all-subtests test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run plan skip-all-subtests test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - trying to skip-all inside Block of 1st-level subtest\n"
        + "ok 2 - trying to skip-all inside Block of 2nd-level subtest\n"
        + "ok 3 - level-one subtest with skip-all\n"
        + "ok 4 - level-one subtest with level-two subtest with skip-all\n";
    assert_eq!(output, expected);
}
