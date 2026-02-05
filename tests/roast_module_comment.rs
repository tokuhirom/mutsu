use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_module_comment() {
    let program = fs::read_to_string("../roast/S26-documentation/module-comment.t")
        .expect("read roast module-comment test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run module-comment test");
    let expected = "1..1\nok 1 - module + semicolon trailing comment\n";
    assert_eq!(output, expected);
}
