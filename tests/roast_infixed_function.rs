use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_infixed_function() {
    let program = fs::read_to_string("../roast/S03-operators/infixed-function.t")
        .expect("read roast infixed-function test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S03-operators/infixed-function.t");
    let output = interp.run(&program).expect("run infixed-function test");
    let expected = "1..5\n"
        .to_string()
        + "ok 1 - 3 [&atan2] 4 == atan2(3, 4)\n"
        + "ok 2 - 3 R[&atan2] 4 == atan2(4, 3)\n"
        + "ok 3 - ... and you can do it twice\n"
        + "ok 4 - [&sprintf] works\n"
        + "ok 5 - X[&sprint] works\n";
    assert_eq!(output, expected);
}
