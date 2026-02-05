use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_like_unlike() {
    let program = fs::read_to_string("../roast/S24-testing/14-like-unlike.t")
        .expect("read roast like-unlike test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S24-testing/14-like-unlike.t");
    let output = interp.run(&program).expect("run like-unlike test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - `like` can accept non-Str objects (Int)\n"
        + "ok 2 - `like` can accept non-Str objects (custom)\n"
        + "ok 3 - `unlike` can accept non-Str objects (Int)\n"
        + "ok 4 - `unlike` can accept non-Str objects (custom)\n";
    assert_eq!(output, expected);
}
