use super::*;

/// Register a user-declared sub name so it can be recognized as a call without parens.
pub(crate) fn register_user_sub(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.user_subs.insert(name.to_string());
    });
    // Circumfix/postcircumfix subs change how expressions parse, so invalidate memos.
    if name.starts_with("circumfix:") || name.starts_with("postcircumfix:") {
        crate::parser::invalidate_all_memos();
    }
}

pub(crate) fn register_user_infix_assoc(name: &str, assoc: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current
            .infix_assoc
            .insert(name.to_string(), assoc.to_string());
    });
}

/// Resolve a reference operator to its numeric precedence level.
pub(crate) fn resolve_op_precedence(ref_op: &str) -> Option<i32> {
    // Strip &-sigil if present: &infix:<+> -> infix:<+>
    let op = ref_op.strip_prefix('&').unwrap_or(ref_op);

    // Reduction form: &[+] -> +
    if let Some(symbol) = op.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        return resolve_infix_symbol_precedence(symbol);
    }

    // Full categorical name: infix:<+>, prefix:<foo>, etc.
    if let Some(symbol) = op.strip_prefix("infix:<").and_then(|s| s.strip_suffix('>')) {
        return resolve_infix_symbol_precedence(symbol);
    }
    if op.starts_with("prefix:<") || op.starts_with("postfix:<") {
        // A user-defined prefix/postfix may itself carry a precedence trait, and a later
        // `is looser(&postfix:<k>)` has to be relative to *that* level, not to the default
        // one it would have had without the trait.
        return Some(lookup_op_precedence(op).unwrap_or(PREC_PREFIX));
    }

    // Bare symbol: +, *, **, ~
    resolve_infix_symbol_precedence(op)
}

fn resolve_infix_symbol_precedence(symbol: &str) -> Option<i32> {
    match symbol {
        "+" | "-" => Some(PREC_ADDITIVE),
        "*" | "/" | "%" | "div" | "mod" | "gcd" | "lcm" => Some(PREC_MULTIPLICATIVE),
        "**" => Some(PREC_POWER),
        "~" => Some(PREC_CONCAT),
        "but" | "does" => Some(PREC_STRUCTURAL),
        _ => {
            // Check if it's a user-defined op with a registered level
            lookup_op_precedence(&format!("infix:<{}>", symbol))
        }
    }
}

/// Register a numeric precedence level for an operator.
pub(crate) fn register_op_precedence(name: &str, level: i32) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.op_precedence.insert(name.to_string(), level);
    });
}

/// Look up the numeric precedence level for an operator.
pub(crate) fn lookup_op_precedence(name: &str) -> Option<i32> {
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            if let Some(v) = scope.op_precedence.get(name) {
                return Some(*v);
            }
        }
        None
    })
}

/// Look up the precedence level for a custom infix word operator by its bare name.
pub(crate) fn lookup_custom_infix_precedence(symbol: &str) -> Option<i32> {
    lookup_op_precedence(&format!("infix:<{}>", symbol))
}

/// Look up the precedence level for a custom prefix operator by its full name.
pub(crate) fn lookup_prefix_precedence(full_name: &str) -> Option<i32> {
    lookup_op_precedence(full_name)
}

/// Look up the precedence level for a custom postfix operator by its full name.
pub(crate) fn lookup_postfix_precedence(full_name: &str) -> Option<i32> {
    lookup_op_precedence(full_name)
}

pub(crate) fn lookup_user_infix_assoc(symbol: &str) -> Option<String> {
    let key = format!("infix:<{}>", symbol);
    SCOPES.with(|s| {
        let scopes = s.borrow();
        for scope in scopes.iter().rev() {
            if let Some(v) = scope.infix_assoc.get(&key) {
                return Some(v.clone());
            }
        }
        None
    })
}

/// Register a user-declared sub with `is test-assertion`.
pub(crate) fn register_user_test_assertion_sub(name: &str) {
    SCOPES.with(|s| {
        let mut scopes = s.borrow_mut();
        let current = scopes
            .last_mut()
            .expect("scope stack should never be empty");
        current.test_assertion_subs.insert(name.to_string());
    });
}

/// Reset all scopes (called at parse start).
pub(crate) fn reset_user_subs() {
    SCOPES.with(|s| {
        *s.borrow_mut() = vec![LexicalScope::default()];
    });
    reset_package_path();
    // Apply pre-seeded operator names (for EVAL context)
    EVAL_OPERATOR_PRESEED.with(|preseed| {
        let names = preseed.borrow();
        for name in names.iter() {
            register_user_sub(name);
            register_user_callable_term_symbol(name);
        }
    });
    EVAL_OPERATOR_ASSOC_PRESEED.with(|preseed| {
        let assoc_map = preseed.borrow();
        for (name, assoc) in assoc_map.iter() {
            register_user_infix_assoc(name, assoc);
        }
    });
    EVAL_IMPORTED_FUNCTION_PRESEED.with(|preseed| {
        let names = preseed.borrow();
        SCOPES.with(|s| {
            let mut scopes = s.borrow_mut();
            if let Some(scope) = scopes.last_mut() {
                for name in names.iter() {
                    scope.imported_functions.insert(name.clone());
                }
            }
        });
    });
    EVAL_USER_SUB_PRESEED.with(|preseed| {
        let names = preseed.borrow();
        SCOPES.with(|s| {
            let mut scopes = s.borrow_mut();
            if let Some(scope) = scopes.last_mut() {
                for name in names.iter() {
                    scope.user_subs.insert(name.clone());
                }
            }
        });
    });
    CURRENT_LANGUAGE_VERSION.with(|v| {
        // Default to 6.d (rakudo's default for a file with no `use vX` pragma).
        // Version-gated behaviors (subset Nil-reset nominalization, grammar
        // `.parse` Failure, class DESTROY / role BUILD submethod dispatch) thus
        // match rakudo's pragma-less default, which is what roast tests assume.
        *v.borrow_mut() = "6.d".to_string();
    });
    // ... unless this is an EVAL, which inherits the calling unit's revision.
    EVAL_LANGUAGE_VERSION_PRESEED.with(|preseed| {
        if let Some(version) = preseed.borrow().as_deref() {
            set_current_language_version(version);
        }
    });
    MONITOR_DECL_ENABLED.with(|v| *v.borrow_mut() = false);
    DECLARE_KEYWORDS.with(|m| m.borrow_mut().clear());
}

/// Seed the language version an EVAL's nested parse starts at. `None` restores
/// the plain 6.d default used for a fresh compilation unit.
pub(crate) fn set_eval_language_version_preseed(version: Option<String>) {
    EVAL_LANGUAGE_VERSION_PRESEED.with(|preseed| {
        *preseed.borrow_mut() = version;
    });
}

pub(crate) fn set_current_language_version(version: &str) {
    CURRENT_LANGUAGE_VERSION.with(|v| {
        *v.borrow_mut() = version.to_string();
    });
}

/// `use OO::Monitors` enables the `monitor` declarator for the rest of the
/// compilation unit (mutsu provides the monitor semantics natively).
pub(crate) fn enable_monitor_decl() {
    MONITOR_DECL_ENABLED.with(|v| *v.borrow_mut() = true);
}

pub(crate) fn monitor_decl_enabled() -> bool {
    MONITOR_DECL_ENABLED.with(|v| *v.borrow())
}

/// Register an EXPORTHOW::DECLARE declarator keyword (from a `use`d module's
/// scan) for the rest of the compilation unit: `keyword Name { ... }` then
/// parses like `class` with the declarator's HOW protocol driving registration.
pub(crate) fn register_declare_keyword(keyword: &str, how_type: &str) {
    DECLARE_KEYWORDS.with(|m| {
        m.borrow_mut()
            .insert(keyword.to_string(), how_type.to_string());
    });
}

/// The currently registered EXPORTHOW::DECLARE declarator keywords.
pub(crate) fn declare_keyword_names() -> Vec<String> {
    DECLARE_KEYWORDS.with(|m| m.borrow().keys().cloned().collect())
}

/// Snapshot / restore the DECLARE keyword table around a nested module scan
/// (the nested parse's reset clears it, and any keywords the scanned module
/// itself imports are lexical to that module, not to the importer).
pub(in crate::parser) fn declare_keywords_snapshot() -> HashMap<String, String> {
    DECLARE_KEYWORDS.with(|m| m.borrow().clone())
}

pub(in crate::parser) fn restore_declare_keywords(saved: HashMap<String, String>) {
    DECLARE_KEYWORDS.with(|m| *m.borrow_mut() = saved);
}

pub(crate) fn current_language_version() -> String {
    CURRENT_LANGUAGE_VERSION.with(|v| v.borrow().clone())
}
