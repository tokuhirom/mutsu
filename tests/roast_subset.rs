use std::fs;
use std::path::Path;

use mutsu::Interpreter;

#[test]
fn roast_subset_programs() {
    let root = Path::new("tests/roast_subset");
    let mut entries: Vec<_> = fs::read_dir(root)
        .expect("read_dir")
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().extension().and_then(|s| s.to_str()) == Some("p6"))
        .collect();

    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let program = fs::read_to_string(&path).expect("read program");
        let out_path = path.with_extension("out");
        let expected = fs::read_to_string(&out_path)
            .unwrap_or_else(|_| panic!("missing expected output: {}", out_path.display()));

        let mut interp = Interpreter::new();
        let output = interp
            .run(&program)
            .unwrap_or_else(|err| panic!("{}: {}", path.display(), err.message));

        assert_eq!(output, expected, "{}", path.display());
    }
}
