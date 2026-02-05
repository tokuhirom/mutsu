use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_usedenv() {
    let program = fs::read_to_string("../roast/S02-magicals/78258.t")
        .expect("read roast UsedEnv test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run UsedEnv test");
    let expected = "";
    assert_eq!(output, expected);
}
