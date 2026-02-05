use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_slangs() {
    let program = fs::read_to_string("../roast/S28-named-variables/slangs.t")
        .expect("read roast slangs test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run slangs test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - $~MAIN is defined\n"
        + "ok 2 - $~Quote is defined\n"
        + "ok 3 - $~Regex is defined\n"
        + "ok 4 - $~P5Regex is defined\n";
    assert_eq!(output, expected);
}
