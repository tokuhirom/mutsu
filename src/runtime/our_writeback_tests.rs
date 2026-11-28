//! A module's own `our` variables never enter the retain-on-miss caller-var
//! writeback set (#19324).
//!
//! The module mainline writes `our @X` through its package key (`@M::X`),
//! so the name reaches the chunk's `free_var_writes`. Recorded as a caller-var
//! writeback, no frame ever claims it -- no local slot carries a package key
//! -- and every later call return had to look at it: a module exporting a few
//! hundred `our` arrays made each closure call in the importer ~100x slower.

use super::Interpreter;
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

#[test]
fn module_our_variables_are_not_left_pending() {
    // Module loading runs the module body nested, which is stack-heavy in a
    // debug build (same precedent as `circular_module_dependency_is_reported`).
    std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(|| {
            let uniq = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let dir = std::env::temp_dir().join(format!("mutsu-our-writeback-{uniq}"));
            fs::create_dir_all(&dir).unwrap();
            let mod_path = dir.join("OurMany.rakumod");
            let mut src = String::from("unit module OurMany;\n");
            for i in 0..20 {
                src.push_str(&format!("our @X{i} is export = 1, 2, 3;\n"));
            }
            src.push_str("our %Table is export = a => 1;\n");
            fs::write(&mod_path, src).unwrap();

            let mut interp = Interpreter::new();
            let program = format!(
                "use lib '{}'; use OurMany; say @X3.elems + %Table<a>;",
                dir.to_string_lossy()
            );
            let output = interp.run(&program).unwrap();
            assert_eq!(output, "4\n");
            let left: Vec<&String> = interp
                .pending_caller_var_writeback
                .iter()
                .filter(|name| name.contains("OurMany::"))
                .collect();
            assert!(
                left.is_empty(),
                "the module's own `our` variables were left pending: {left:?}"
            );

            let _ = fs::remove_file(mod_path);
            let _ = fs::remove_dir(dir);
        })
        .unwrap()
        .join()
        .unwrap();
}
