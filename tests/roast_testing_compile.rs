use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_testing_compile() {
    let program = fs::read_to_string("../roast/S24-testing/0-compile.t")
        .expect("read roast 0-compile test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S24-testing/0-compile.t");
    let output = interp.run(&program).expect("run 0-compile test");
    let expected = "1..1\nok 1\n";
    assert_eq!(output, expected);
}
