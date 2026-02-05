use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_subname() {
    let program = fs::read_to_string("../roast/S02-magicals/subname.t")
        .expect("read roast subname test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run subname test");
    let expected = "1..4\n"
        .to_string()
        + "ok 1 - got the right routine name in the default package\n"
        + "ok 2 - got the right routine name outside the default package\n"
        + "ok 3 - got an empty string for an anon block\n"
        + "ok 4 - &?ROUTINE not available outside of a routine\n";
    assert_eq!(output, expected);
}
