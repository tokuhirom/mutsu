use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_bom_test() {
    let program = fs::read_to_string("../roast/S02-lexical-conventions/bom.t")
        .expect("read roast bom test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run bom test");
    let expected = "1..1\nok 1 - can parse a file starting with a byte-order mark\n";
    assert_eq!(output, expected);
}
