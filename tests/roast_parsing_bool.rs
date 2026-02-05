use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_parsing_bool() {
    let program = fs::read_to_string("../roast/S02-types/parsing-bool.t")
        .expect("read roast parsing-bool test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-types/parsing-bool.t");
    let output = interp.run(&program).expect("run parsing-bool test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - Bool::False as RHS\n"
        + "ok 2 - Bool::False as LHS\n"
        + "ok 3 - False as RHS\n"
        + "ok 4 - False as LHS\n";
    assert_eq!(output, expected);
}
