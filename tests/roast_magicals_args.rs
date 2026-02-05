use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_magicals_args() {
    let program = fs::read_to_string("../roast/S02-magicals/args.t")
        .expect("read roast args test");
    let mut interp = Interpreter::new();
    let output = interp.run(&program).expect("run args test");
    let expected = "1..6\n"
        .to_string()
        + "ok 1 - @*ARGS is an Array\n"
        + "ok 2 - by default @*ARGS is empty array\n"
        + "ok 3 - @*ARGS is writable\n"
        + "ok 4 - providing command line arguments sets @*ARGS\n"
        + "ok 5 - postcircumfix:<[ ]> works for @*ARGS\n"
        + "ok 6 - can copy @*ARGS to array.\n";
    assert_eq!(output, expected);
}
