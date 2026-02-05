use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_progname() {
    let program = fs::read_to_string("../roast/S02-magicals/progname.t")
        .expect("read roast progname test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-magicals/progname.t");
    let output = interp.run(&program).expect("run progname test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - progname var matches test file path\n"
        + "ok 2 - progname var accessible as context var\n"
        + "ok 3 - $*PROGRAM-NAME is assignable\n"
        + "ok 4 - $*PROGRAM-NAME is not confused by compiler options\n";
    assert_eq!(output, expected);
}
