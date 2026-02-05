use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_group() {
    let program = fs::read_to_string("../roast/S02-magicals/GROUP.t")
        .expect("read roast GROUP test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-magicals/GROUP.t");
    let output = interp.run(&program).expect("run GROUP test");
    let expected = "1..1\nok 1 - .WHAT on $*GROUP after using $*GROUP values lives\n";
    assert_eq!(output, expected);
}
