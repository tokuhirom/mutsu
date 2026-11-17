//! The importer's parse reads the unit a `use` loaded (ADR-17558 §2.2).
//!
//! A compiling parse loads a `use`d module when it reaches the statement
//! (`crate::compile_time_loader`), so what the module provides is known from
//! the module itself rather than predicted: routines whose names a module
//! computes at run time (`OUR::{'&postfix:<' ~ $code ~ '>'} := ...` in a loop,
//! Moneys, #9500), the precedence an operator declared, and the declarators an
//! `EXPORTHOW` merge at BEGIN time gave the module (CompUnit::Util's
//! `re-exporthow`, #17557). These replace the parse-time probe of ADR-0124 /
//! ADR-17557, which ran such a module in a second interpreter to learn them.
//!
//! The static scan still supplies everything else, and is all an analysis
//! parse (which loads nothing) has.

use super::InlineModuleExport;

/// Register in the current scope what the loaded unit of `module` (its bare
/// name, no dist selectors) provides to a `use` importing `import_tags`
/// (`None`: every tag, the `use`'s arguments go to a `sub EXPORT`). Does
/// nothing when no loader is reachable.
// Cost: O(e * n + d), e = exported routines, n = built-in operator count,
// d = declarators.
pub(crate) fn register_loaded_unit_facts(module: &str, import_tags: Option<&[String]>) {
    let Some(facts) = crate::compile_time_loader::loaded_facts(module, import_tags) else {
        return;
    };
    let exports: Vec<InlineModuleExport> = facts
        .exports
        .into_iter()
        .map(|export| {
            let (precedence, associativity) = parser_precedence(export.op_prec.as_ref());
            InlineModuleExport {
                name: export.name,
                precedence,
                associativity,
                is_test_assertion: false,
            }
        })
        .collect();
    super::apply_module_exports(&exports);
    for (directive, keyword) in &facts.declarators {
        match directive.as_str() {
            "DECLARE" => super::register_scoped_declare_keyword(module, keyword),
            "SUPERSEDE" => super::register_scoped_supersede_keyword(module, keyword),
            _ => {}
        }
    }
}

/// The parser's numeric precedence level and associativity for a loaded
/// operator's `Routine.prec` hash: the level of the built-in operator it was
/// derived from, moved by five per `is tighter` / `is looser` step.
// Cost: O(n), n = built-in operator count.
fn parser_precedence(prec: Option<&crate::op_prec::OpPrec>) -> (Option<i32>, Option<String>) {
    let Some(prec) = prec else {
        return (None, None);
    };
    let field = |key: &str| {
        prec.entries()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.to_string())
    };
    let level = field("prec").and_then(|level| {
        let (base, steps) = crate::op_prec::split_prec_marks(&level);
        crate::op_prec::infix_symbols_with_prec(&base)
            .find_map(super::super::registry::resolve_op_precedence)
            .map(|anchor| anchor + 5 * steps)
    });
    let assoc = field("assoc").filter(|assoc| assoc != "left");
    (level, assoc)
}
