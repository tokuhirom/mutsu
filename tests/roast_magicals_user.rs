use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_user() {
    let program = fs::read_to_string("../roast/S02-magicals/USER.t")
        .expect("read roast USER test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run USER test");
    let expected = "1..1\nok 1 - .WHAT on $*USER after using $*USER values lives\n";
    assert_eq!(output, expected);
}
