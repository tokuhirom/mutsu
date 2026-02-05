use std::fs;

use mutsu::Interpreter;

#[test]
fn roast_grapheme_break_test() {
    let program = fs::read_to_string("../roast/S15-nfg/grapheme-break.t")
        .expect("read roast grapheme-break test");
    let mut interp = Interpreter::new();
    interp.set_program_path("../roast/S15-nfg/grapheme-break.t");
    let output = interp.run(&program).expect("run grapheme-break test");
    let expected = "1..1\nok 1 - dummy test file\n";
    assert_eq!(output, expected);
}
