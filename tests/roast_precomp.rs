use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_precomp() {
    let program = fs::read_to_string("../roast/S12-traits/precomp.t")
        .expect("read roast precomp test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run precomp test");
    let expected = "1..1\nok 1 - did we compile and execute ok\n";
    assert_eq!(output, expected);
}
