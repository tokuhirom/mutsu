use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_io_other() {
    let program = fs::read_to_string("../roast/S32-io/other.t")
        .expect("read roast io other test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S32-io/other.t");
    let output = interp.run(&program).expect("run io other test");
    let expected = "1..1\nok 1 - IO::Special:U.Str does not crash\n";
    assert_eq!(output, expected);
}
