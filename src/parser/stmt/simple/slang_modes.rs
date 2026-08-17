//! Slang activation parser modes (ADR-0026 §2.3).
//!
//! A slang-activating `use` (e.g. `use Slang::Tuxic;`) maps the grammar
//! rules its roles override onto these mode flags for the remainder of the
//! current compilation unit. The flags are unit-scoped parser state modeled
//! on `CURRENT_LANGUAGE_VERSION` / `DECLARE_KEYWORDS`: cleared at parse
//! start (`reset_user_subs`) and snapshot/restored around nested module
//! scans. Slang state does not leak into EVAL strings or importing units.

use std::cell::Cell;

#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
pub(crate) struct SlangModes {
    /// `term:sym<identifier>` override: an identifier followed by
    /// whitespace and `(` parses as a call with the parenthesized
    /// arguments (Tuxic's `foo (42)`), except for control keywords and
    /// known type names.
    pub spaced_call: bool,
    /// `methodop` override: `.method (args)` parses as a method call with
    /// the parenthesized arguments instead of the "no space allowed
    /// between method name and the left parenthesis" error.
    pub spaced_methodop: bool,
    /// `identifier`/`name` override (Slangify's Piersing fixture): a bare
    /// identifier used as a sub-declaration name or a bareword call/term may
    /// end in a single trailing `?`/`!` (e.g. `sub pass?(|c) {...}`, called
    /// as `pass? "..."`). Scoped to those two grammar productions rather
    /// than the shared low-level identifier scanner, so it does not disturb
    /// a sigiled variable's own identifier parsing — `$x?` (an optional
    /// signature parameter) must keep meaning "the variable `x`, marked
    /// optional", not "the variable `x?`".
    pub ident_trailing_punct: bool,
}

thread_local! {
    static SLANG_MODES: Cell<SlangModes> = const { Cell::new(SlangModes {
        spaced_call: false,
        spaced_methodop: false,
        ident_trailing_punct: false,
    }) };
}

pub(crate) fn slang_modes() -> SlangModes {
    SLANG_MODES.with(|m| m.get())
}

/// Enable/replace the slang mode set for the remainder of the current
/// compilation unit. Callers that flip modes mid-parse must also reset the
/// statement/term memo tables (a memoized parse from before the flip would
/// otherwise be replayed under the wrong grammar).
pub(crate) fn set_slang_modes(modes: SlangModes) {
    SLANG_MODES.with(|m| m.set(modes));
}

pub(in crate::parser) fn reset_slang_modes() {
    set_slang_modes(SlangModes::default());
}

/// Snapshot / restore around a nested module scan: the scanned module's
/// slang state is lexical to that module, not to the importer (and vice
/// versa).
pub(in crate::parser) fn slang_modes_snapshot() -> SlangModes {
    slang_modes()
}

pub(in crate::parser) fn restore_slang_modes(saved: SlangModes) {
    set_slang_modes(saved);
}

pub(crate) fn slang_spaced_call() -> bool {
    slang_modes().spaced_call
}

pub(crate) fn slang_spaced_methodop() -> bool {
    slang_modes().spaced_methodop
}

pub(crate) fn slang_ident_trailing_punct() -> bool {
    slang_modes().ident_trailing_punct
}

/// Consume a single trailing `?`/`!` from `rest` for the `ident_trailing_punct`
/// mode, appending it to `name`. A no-op when the mode is off. Guarded
/// against eating one half of a doubled `??`/`!!` (e.g. a compact ternary
/// `cond??a!!b` immediately after an identifier with no separating
/// whitespace) by refusing to consume when the same character repeats.
pub(crate) fn consume_slang_ident_trailing_punct<'a>(name: &mut String, rest: &'a str) -> &'a str {
    if !slang_ident_trailing_punct() {
        return rest;
    }
    let mut chars = rest.chars();
    match chars.next() {
        Some(c @ ('?' | '!')) if chars.next() != Some(c) => {
            name.push(c);
            &rest[c.len_utf8()..]
        }
        _ => rest,
    }
}

/// The recognized-override map (ADR-0026 §2.3): apply one overridden
/// grammar-rule name from a slang role to the mode set. Returns `None` when
/// the rule is not supported — the caller must raise a hard compile-time
/// error naming the rule (an unknown override means the slang's semantics
/// would be silently wrong), never ignore it.
pub(crate) fn apply_slang_rule_override(modes: &mut SlangModes, rule: &str) -> Option<()> {
    match rule {
        "term:sym<identifier>" => modes.spaced_call = true,
        "methodop" => modes.spaced_methodop = true,
        // Tuxic's override re-states the stock sub-declarator rule so the
        // spaced form composes with sub declarations; stock mutsu already
        // accepts `sub foo ($x) { }`, so this maps to a no-op.
        "routine-declarator:sym<sub>" | "routine_declarator:sym<sub>" => {}
        // Slangify's Piersing fixture: identifiers/names may end in a
        // trailing `?`/`!` (`sub pass?(|c) {...}`, called as `pass? "..."`).
        "identifier" | "name" => modes.ident_trailing_punct = true,
        _ => return None,
    }
    Some(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, Stmt};

    const TUXIC: SlangModes = SlangModes {
        spaced_call: true,
        spaced_methodop: true,
        ident_trailing_punct: false,
    };

    fn parse_with_modes(modes: SlangModes, input: &str) -> Result<Vec<Stmt>, String> {
        crate::parser::stmt::simple::reset_user_subs();
        crate::parser::stmt::reset_statement_memo();
        set_slang_modes(modes);
        let result = crate::parser::stmt::program(input);
        reset_slang_modes();
        match result {
            Ok((rest, stmts)) if rest.trim().is_empty() => Ok(stmts),
            Ok((rest, _)) => Err(format!("unparsed trailing input: {rest:?}")),
            Err(e) => Err(format!("{e:?}")),
        }
    }

    fn first_expr(stmts: &[Stmt]) -> &Expr {
        stmts
            .iter()
            .find_map(|s| match s {
                Stmt::Expr(e) => Some(e),
                _ => None,
            })
            .expect("expected an expression statement")
    }

    #[test]
    fn spaced_call_mode_parses_identifier_ws_paren_as_call() {
        let stmts = parse_with_modes(TUXIC, "foo (1, 2);").unwrap();
        match first_expr(&stmts) {
            Expr::Call { name, args } => {
                assert_eq!(name.as_str(), "foo");
                assert_eq!(
                    args.len(),
                    2,
                    "spaced call must take the paren contents as an arg list, got {args:?}"
                );
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn spaced_call_off_keeps_listop_semantics() {
        let stmts = parse_with_modes(SlangModes::default(), "foo (1, 2);").unwrap();
        match first_expr(&stmts) {
            Expr::Call { name, args } => {
                assert_eq!(name.as_str(), "foo");
                assert_eq!(
                    args.len(),
                    1,
                    "stock grammar: one parenthesized List argument"
                );
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn spaced_methodop_mode_parses_method_ws_paren_as_call() {
        let stmts = parse_with_modes(TUXIC, "$x.blah (1, 2);").unwrap();
        match first_expr(&stmts) {
            Expr::MethodCall { name, args, .. } => {
                assert_eq!(name.as_str(), "blah");
                assert_eq!(args.len(), 2);
            }
            other => panic!("expected MethodCall, got {other:?}"),
        }
    }

    #[test]
    fn spaced_methodop_off_stays_an_error() {
        assert!(parse_with_modes(SlangModes::default(), "$x.blah (1, 2);").is_err());
    }

    #[test]
    fn spaced_private_methodop_mode_parses_bang_ws_paren_as_call() {
        let stmts = parse_with_modes(TUXIC, "self!ready (0, 2);").unwrap();
        match first_expr(&stmts) {
            Expr::MethodCall {
                name,
                args,
                modifier,
                ..
            } => {
                assert_eq!(name.as_str(), "ready");
                assert_eq!(args.len(), 2);
                assert_eq!(*modifier, Some('!'));
            }
            other => panic!("expected private MethodCall, got {other:?}"),
        }
    }

    #[test]
    fn spaced_private_methodop_off_unchanged() {
        // Stock grammar: `self!ready` is a no-arg private call; the
        // parenthesized list does not attach to it as arguments.
        if let Ok(stmts) = parse_with_modes(SlangModes::default(), "self!ready (0, 2);") {
            if let Expr::MethodCall { args, .. } = first_expr(&stmts) {
                assert!(args.len() < 2, "stock parse must not bind the spaced args");
            }
        }
    }

    #[test]
    fn spaced_call_mode_excludes_control_keywords() {
        let stmts = parse_with_modes(TUXIC, "if (1) { 2 }").unwrap();
        assert!(
            stmts.iter().any(|s| matches!(s, Stmt::If { .. })),
            "`if (1) {{ }}` must stay an if statement under Tuxic mode, got {stmts:?}"
        );
    }

    #[test]
    fn reset_user_subs_clears_slang_modes() {
        set_slang_modes(TUXIC);
        crate::parser::stmt::simple::reset_user_subs();
        assert_eq!(slang_modes(), SlangModes::default());
    }

    #[test]
    fn rule_override_map_matches_adr_0026() {
        let mut modes = SlangModes::default();
        assert!(apply_slang_rule_override(&mut modes, "term:sym<identifier>").is_some());
        assert!(apply_slang_rule_override(&mut modes, "methodop").is_some());
        assert!(apply_slang_rule_override(&mut modes, "routine-declarator:sym<sub>").is_some());
        assert!(apply_slang_rule_override(&mut modes, "routine_declarator:sym<sub>").is_some());
        assert_eq!(
            modes,
            SlangModes {
                spaced_call: true,
                spaced_methodop: true,
                ident_trailing_punct: false,
            }
        );
        let mut fresh = SlangModes::default();
        assert!(apply_slang_rule_override(&mut fresh, "term:sym<colonpair>").is_none());
        assert_eq!(fresh, SlangModes::default());
    }

    #[test]
    fn rule_override_maps_identifier_and_name_to_trailing_punct() {
        let mut modes = SlangModes::default();
        assert!(apply_slang_rule_override(&mut modes, "identifier").is_some());
        assert!(modes.ident_trailing_punct);
        let mut modes2 = SlangModes::default();
        assert!(apply_slang_rule_override(&mut modes2, "name").is_some());
        assert!(modes2.ident_trailing_punct);
    }

    #[test]
    fn ident_trailing_punct_mode_extends_sub_call_name() {
        let modes = SlangModes {
            ident_trailing_punct: true,
            ..SlangModes::default()
        };
        let stmts = parse_with_modes(modes, "pass? \"ok\";").unwrap();
        match first_expr(&stmts) {
            Expr::Call { name, .. } => assert_eq!(name.as_str(), "pass?"),
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn ident_trailing_punct_off_leaves_call_name_unsuffixed() {
        // Without the mode, a bare `?` after an identifier is not part of the
        // name: `pass` parses as its own bareword term/statement, and the
        // `?"ok"` that follows parses separately as the stock boolean-coercion
        // prefix operator — never as a single call named "pass?".
        let stmts = parse_with_modes(SlangModes::default(), "pass? \"ok\";").unwrap();
        assert!(
            !stmts.iter().any(
                |s| matches!(s, Stmt::Expr(Expr::Call { name, .. }) if name.as_str() == "pass?")
            ),
            "must not parse as a single Call named \"pass?\", got {stmts:?}"
        );
    }

    #[test]
    fn ident_trailing_punct_mode_does_not_eat_one_half_of_a_compact_ternary() {
        let modes = SlangModes {
            ident_trailing_punct: true,
            ..SlangModes::default()
        };
        // `cond??a!!b` (no surrounding whitespace) is a compact ternary; the
        // identifier scan must not consume one `?` of the doubled `??` as a
        // trailing-punct suffix.
        let stmts = parse_with_modes(modes, "my $x = 1; $x??2!!3;").unwrap();
        assert!(
            stmts
                .iter()
                .any(|s| matches!(s, Stmt::Expr(Expr::Ternary { .. }))),
            "expected a Ternary expression, got {stmts:?}"
        );
    }
}
