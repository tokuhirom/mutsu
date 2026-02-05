use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_cmp_ok() {
    let program = fs::read_to_string("../roast/S24-testing/13-cmp-ok.t")
        .expect("read roast cmp-ok test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S24-testing/13-cmp-ok.t");
    let output = interp.run(&program).expect("run cmp-ok test");
    let expected = "1..2\n"
        .to_string()
        + "ok 1 - cmp-ok makes $expected more presentable than just its .Str\n"
        + "ok 2 - can use cmp-ok with `=:=` operator\n";
    assert_eq!(output, expected);
}
