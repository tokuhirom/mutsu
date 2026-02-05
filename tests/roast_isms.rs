use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_isms() {
    let program = fs::read_to_string("../roast/S01-perl-5-integration/isms.t")
        .expect("read roast isms test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run isms test");
    let expected = "1..2\nok 1 - does =~ survive?\nok 2 - did it actually do the assignment?\n";
    assert_eq!(output, expected);
}
