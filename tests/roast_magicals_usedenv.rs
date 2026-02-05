use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_usedenv() {
    let program = fs::read_to_string("../roast/S02-magicals/78258.t")
        .expect("read roast UsedEnv test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-magicals/78258.t");
    let output = interp.run(&program).expect("run UsedEnv test");
    let expected = "1..1\nok 1 - env exists in use (RT #78258)\n";
    assert_eq!(output, expected);
}
