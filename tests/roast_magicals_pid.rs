use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_pid() {
    let program = fs::read_to_string("../roast/S02-magicals/pid.t")
        .expect("read roast pid test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S02-magicals/pid.t");
    let output = interp.run(&program).expect("run pid test");
    let expected = "1..2\nok 1 - my $*PID is different from a child $*PID\nok 2\n";
    assert_eq!(output, expected);
}
