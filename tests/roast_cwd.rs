use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_cwd() {
    let program = fs::read_to_string("../roast/S16-io/cwd.t")
        .expect("read roast cwd test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run cwd test");
    let expected = "1..3\nok 1\nok 2 - $*CWD.raku works\nok 3 - $*CWD.gist works\n";
    assert_eq!(output, expected);
}
