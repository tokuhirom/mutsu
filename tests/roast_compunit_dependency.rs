use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_compunit_dependency() {
    let program = fs::read_to_string("../roast/S11-compunit/compunit-dependencyspecification.t")
        .expect("read roast compunit dependency test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S11-compunit/compunit-dependencyspecification.t");
    let output = interp.run(&program).expect("run compunit dependency test");
    let expected = "1..6\nok 1\nok 2\nok 3\nok 4\nok 5\nok 6\n";
    assert_eq!(output, expected);
}
