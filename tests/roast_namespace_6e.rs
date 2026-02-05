use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_namespace_6e() {
    let program = fs::read_to_string("../roast/S05-grammar/namespace-6e.t")
        .expect("read roast namespace-6e test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run namespace-6e test");
    let expected = "1..1\nok 1 - we can create grammar named Grammar\n";
    assert_eq!(output, expected);
}
