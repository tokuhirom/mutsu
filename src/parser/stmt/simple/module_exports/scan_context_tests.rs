//! A module scan is a pure function of the module sources: it is cached on
//! disk and served to every later importer, so nothing a nested scan does may
//! leak into the parse that requested it (#18848).

/// A scanned dependency's block-form package (`our module NativeCall { }`)
/// is that module's declaration, not the importer's: the importer's later
/// `unit module` must not become too late. When it did, the importer's own
/// scan dropped its `unit module Notcurses::Native::Types;`, and the cache
/// recorded the module's exported types under GLOBAL. Every program that
/// read the warmed cache afterwards saw a second `NcplaneHandle`.
#[test]
fn a_scanned_block_package_is_not_the_importers_declaration() {
    crate::parser::stmt::simple::reset_package_path();
    assert!(!crate::parser::stmt::simple::package_declared());
    let dep = super::scan_module_source("our module ScDep { }\nclass ScDep::K { }\n", "<dep>");
    assert!(dep.type_names.iter().any(|n| n == "ScDep::K"));
    assert!(
        !crate::parser::stmt::simple::package_declared(),
        "the dependency's `our module` leaked into the importer's unit"
    );
}

/// The same leak through a real nested scan: the `unit module` that follows
/// a `use` of a module declaring a block package keeps the unit package of
/// its exported types.
#[test]
fn a_unit_module_after_using_a_block_package_keeps_its_package() {
    let dir = std::env::temp_dir().join(format!("mutsu-scan-context-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("ScCtxDep.rakumod"), "our module ScCtxDep { }\n").unwrap();
    let saved = crate::parser::stmt::simple::parser_lib_paths();
    crate::parser::stmt::simple::set_parser_lib_paths(vec![dir.to_string_lossy().into_owned()]);
    crate::parser::stmt::simple::reset_package_path();
    let scan = super::scan_module_source(
        "use ScCtxDep;\nunit module ScCtxTy;\nclass ScH is export { }\n",
        "<unit>",
    );
    crate::parser::stmt::simple::set_parser_lib_paths(saved);
    let _ = std::fs::remove_dir_all(&dir);
    let qualified: Vec<&str> = scan
        .exported_types
        .iter()
        .map(|t| t.qualified.as_str())
        .collect();
    assert_eq!(qualified, ["ScCtxTy::ScH"]);
}
