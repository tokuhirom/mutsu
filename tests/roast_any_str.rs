use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_any_str() {
    let program = fs::read_to_string("../roast/S03-smartmatch/any-str.t")
        .expect("read roast any-str test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run any-str test");
    let expected = "1..5\n"
        .to_string()
        + "ok 1 - !(foo ne foo)\n"
        + "ok 2 - bar ne foo)\n"
        + "ok 3 - string equality\n"
        + "ok 4 - negated string equality\n"
        + "ok 5 - Mu !~~ \"\"\n";
    assert_eq!(output, expected);
}
