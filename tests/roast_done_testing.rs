use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_done_testing() {
    let program = fs::read_to_string("../roast/S24-testing/6-done_testing.t")
        .expect("read roast done-testing test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run done-testing test");
    let expected = "ok 1\nok 2\nnot ok 3 # TODO\n1..3\n";
    assert_eq!(output, expected);
}
