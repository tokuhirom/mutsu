use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_any_complex() {
    let program = fs::read_to_string("../roast/S03-smartmatch/any-complex.t")
        .expect("read roast any-complex test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S03-smartmatch/any-complex.t");
    let output = interp.run(&program).expect("run any-complex test");
    let expected = "1..12\n"
        .to_string()
        + "ok 1 - Complex  ~~ Complex (+)\n"
        + "ok 2 - Complex  ~~ Complex (-)\n"
        + "ok 3 - Complex  ~~ Complex (-)\n"
        + "ok 4 - Complex !~~ Complex (-)\n"
        + "ok 5 - Complex !~~ Complex (+)\n"
        + "ok 6 - Complex !~~ Complex (+)\n"
        + "ok 7 - Num  ~~ Complex (+)\n"
        + "ok 8 - Num  ~~ Complex (-)\n"
        + "ok 9 - Num  ~~ Complex (-)\n"
        + "ok 10 - Num !~~ Complex (-)\n"
        + "ok 11 - Num !~~ Complex (+)\n"
        + "ok 12 - Num !~~ Complex (+)\n";
    assert_eq!(output, expected);
}
