use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_plan_skip_all() {
    let program = fs::read_to_string("../roast/S24-testing/11-plan-skip-all.t")
        .expect("read roast plan skip-all test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run plan skip-all test");
    let expected = "1..0 # SKIP Testing skippage of `plan skip-all`\n";
    assert_eq!(output, expected);
}
