use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_force_todo() {
    let program = fs::read_to_string("../roast/S24-testing/2-force_todo.t")
        .expect("read roast force_todo test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S24-testing/2-force_todo.t");
    let output = interp.run(&program).expect("run force_todo test");
    let expected = "1..12\n"
        .to_string()
        + "not ok 1 - This will fail, but will be forced-TODO by force_todo() # TODO\n"
        + "ok 2 - This will pass normally\n"
        + "not ok 3 - This will fail, but will be forced-TODO by force_todo() # TODO\n"
        + "ok 4 - This will pass normally\n"
        + "not ok 5 - This will TODO fail, and will be forced-TODO by force_todo() # TODO\n"
        + "ok 6 - This will pass normally\n"
        + "not ok 7 - This will fail, and will be forced-TODO by force_todo() # TODO\n"
        + "not ok 8 - This will fail, and will be forced-TODO by force_todo() # TODO\n"
        + "not ok 9 - This will fail, and will be forced-TODO by force_todo() # TODO\n"
        + "ok 10 - This will pass normally\n"
        + "not ok 11 - This will fail, and will be forced-TODO by force_todo() # TODO\n"
        + "ok 12 - force_todo is implemented\n";
    assert_eq!(output, expected);
}
