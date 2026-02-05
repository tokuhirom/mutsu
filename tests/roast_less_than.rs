use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_less_than() {
    let program = fs::read_to_string("../roast/S02-one-pass-parsing/less-than.t")
        .expect("read roast less-than test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run less-than test");
    let expected = "1..9\n"
        .to_string()
        + "ok 1 - random numbers are greater than or equal to 0\n"
        + "ok 2 - random numbers are less than 1\n"
        + "ok 3 - 3 is greater than 0\n"
        + "ok 4 - ~<...> is parsed correctly\n"
        + "ok 5 - +<...> is parsed correctly\n"
        + "ok 6 - ?<...> is parsed correctly\n"
        + "ok 7 - ~(<...>) is parsed correctly\n"
        + "ok 8 - +(<...>) is parsed correctly\n"
        + "ok 9 - ?(<...>) is parsed correctly\n";
    assert_eq!(output, expected);
}
