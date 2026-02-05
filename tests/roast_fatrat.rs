use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_fatrat() {
    let program = fs::read_to_string("../roast/S02-types/fatrat.t")
        .expect("read roast fatrat test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-types/fatrat.t");
    let output = interp.run(&program).expect("run fatrat test");
    let expected = "1..1\nok 1\n";
    assert_eq!(output, expected);
}
