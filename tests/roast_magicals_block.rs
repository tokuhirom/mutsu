use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_block() {
    let program = fs::read_to_string("../roast/S02-magicals/block.t")
        .expect("read roast block test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-magicals/block.t");
    let output = interp.run(&program).expect("run block test");
    let expected = "1..3\n"
        .to_string()
        + "ok 1 - the &?BLOCK magical worked\n"
        + "ok 2 - Correct result from function generator returning function using &?BLOCK\n"
        + "ok 3 - Correct closure semantics with &?BLOCK\n";
    assert_eq!(output, expected);
}
