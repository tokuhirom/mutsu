use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_bail_out() {
    let program = fs::read_to_string("../roast/S24-testing/7-bail_out.t")
        .expect("read roast bail_out test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run bail_out test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - bail out without a description or plan\n"
        + "ok 2 - no more tests run after bailing out\n"
        + "ok 3 - bail out with description\n"
        + "ok 4 - immediate bail out does not crash\n";
    assert_eq!(output, expected);
}
