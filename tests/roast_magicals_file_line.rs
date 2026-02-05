use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_file_line() {
    let program = fs::read_to_string("../roast/S02-magicals/file_line.t")
        .expect("read roast file_line test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run file_line test");
    let expected = "1..2\n"
        .to_string()
        + "ok 1 - $?LINE works\n"
        + "ok 2 - $?FILE works\n";
    assert_eq!(output, expected);
}
