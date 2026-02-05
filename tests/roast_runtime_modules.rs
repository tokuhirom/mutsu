use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_runtime_modules() {
    let program = fs::read_to_string("../roast/S11-modules/runtime.t")
        .expect("read roast runtime test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S11-modules/runtime.t");
    let output = interp.run(&program).expect("run runtime test");
    let expected = "1..1\nok 1 - Runtime package creation doesn't screw up module loading\n";
    assert_eq!(output, expected);
}
