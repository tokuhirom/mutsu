use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_until() {
    let program = fs::read_to_string("../roast/S04-statements/until.t")
        .expect("read roast until test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S04-statements/until.t");
    let output = interp.run(&program).expect("run until test");
    let expected = "\
1..4
ok 1 - until $i >= 5 {} works
ok 2 - until 5 <= $i {} works
ok 3 - until ($i >= 5) {} works
ok 4 - until (5 <= $i) {} works
";
    assert_eq!(output, expected);
}
