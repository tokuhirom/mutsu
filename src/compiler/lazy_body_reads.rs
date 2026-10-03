//! The op-level scans behind `lazy_body_env_sync`: whether a compiled body's
//! by-name reads are enumerable, and which names they are.

use crate::ast::{Expr, Stmt};
use crate::opcode::{CompiledCode, CompiledDeclExpr, OpCode};
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// Push `sym` unless `out` already holds it.
// Cost: O(n), n = out.len().
pub(super) fn push_unique(out: &mut Vec<Symbol>, sym: Symbol) {
    if !out.contains(&sym) {
        out.push(sym);
    }
}

/// Fold the by-name reads of a compiled declaration chunk; `None` when it
/// reads names no op scan can bound.
// Cost: O(b), b = ops and constants of `chunk` and its nested closures.
pub(super) fn chunk_reads(chunk: &CompiledDeclExpr, out: &mut Vec<Symbol>) -> Option<()> {
    if !lazy_body_reads_bounded(&chunk.code) {
        return None;
    }
    collect_by_name_reads(&chunk.code, out);
    Some(())
}

/// Push every identifier in a type-name spelling (`Foo::Bar[Int]:D`, a
/// parent string with bracketed arguments) as a by-name read: a lexical type
/// or constant the declaration names (`my constant T = Int; class C is T`)
/// is looked up by name at registration. Over-approximating with unrelated
/// tokens only keeps an extra mirror live.
// Cost: O(n), n = type_name.len().
pub(super) fn push_type_name_tokens(type_name: &str, out: &mut Vec<Symbol>) {
    let mut push = |token: &str| {
        if !token.is_empty() {
            push_unique(out, Symbol::intern(token));
        }
    };
    let is_name_char = |c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':');
    for raw in type_name.split(|c: char| !is_name_char(c)) {
        push(raw.trim_matches(':'));
        for part in raw.split(':') {
            push(part);
        }
    }
}

/// Fold the by-name reads of a `token`/`rule` body (ADR-0009: the regex is
/// interpreter-executed, so there are no ops to scan). Only a static pattern
/// is enumerable: no variable interpolation, no `"..."` thunk, no code block
/// and no `&` call, so the one name channel left is a subrule `<foo>`, which
/// may resolve the lexical `&foo` — every identifier is pushed as such.
/// `None` for any other body shape.
// Cost: O(p), p = total pattern length of the body.
pub(super) fn token_body_reads(body: &[Stmt], out: &mut Vec<Symbol>) -> Option<()> {
    for stmt in body {
        let Stmt::Expr(Expr::Literal(value)) = stmt else {
            return None;
        };
        let pattern = match value.view() {
            ValueView::Regex(p) => p.as_str().to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.as_str().to_string(),
            _ => return None,
        };
        if !crate::runtime::regex_parse::regex_pattern_is_static(&pattern)
            || pattern.contains(['{', '&'])
        {
            return None;
        }
        let is_name_char = |c: char| c.is_alphanumeric() || matches!(c, '_' | '-');
        for word in pattern.split(|c: char| !is_name_char(c)) {
            if word.starts_with(|c: char| c.is_alphabetic() || c == '_') {
                push_unique(out, Symbol::intern(&format!("&{word}")));
            }
        }
    }
    Some(())
}

/// Whether every by-name read of `code` (and of each closure nested in it)
/// is visible to [`collect_by_name_reads`]. An interpolating or indirect
/// regex, a dynamic substitution replacement, a deferred phaser, and a
/// nested declaration that is not itself bounded all resolve names the op
/// scan cannot enumerate.
// Cost: O(b), b = total ops and constants of `code` and its nested closures.
pub(super) fn lazy_body_reads_bounded(code: &CompiledCode) -> bool {
    let own = code.ops.iter().all(|op| match op {
        // A bounded nested sub's reads are part of `free_var_syms`; a bounded
        // nested class's or role's are carried in `lazy_decl_reads`.
        OpCode::RegisterDecl(idx) => {
            matches!(
                code.decl_plans.get(*idx as usize),
                Some(
                    crate::opcode::CompiledDeclPlanRef::Sub(_)
                        | crate::opcode::CompiledDeclPlanRef::Class(_)
                        | crate::opcode::CompiledDeclPlanRef::Role(_)
                )
            ) && code.bounded_lazy_decl_plans.contains(idx)
        }
        OpCode::PhaserEnd { .. } | OpCode::CheckPhaser { .. } => false,
        _ => true,
    }) && !code.holds_interpolating_regex()
        && !code.holds_dynamic_substitution()
        && !code.holds_indirect_regex_lookup();
    own && code
        .closure_compiled_codes
        .iter()
        .all(|c| lazy_body_reads_bounded(c))
}

/// Every name `code` may resolve by name in an enclosing frame's env: its
/// free reads and writes (which already include its nested closures', nested
/// routines' and `gather`/`whenever` bodies'), the rw-arg-sink targets, the
/// reads of its bounded nested class/role declarations, and — at any closure
/// depth — the scalars it mutates in place and the bare callee names (a call
/// `e()` colliding with an outer `my $e` reads `env[e]`).
// Cost: O(b), b = total ops of `code` and its nested closures.
pub(super) fn collect_by_name_reads(code: &CompiledCode, out: &mut Vec<Symbol>) {
    for sym in code
        .free_var_syms
        .iter()
        .chain(&code.free_var_writes)
        .chain(&code.free_var_container_writes)
        .chain(&code.rw_arg_env_sync_syms)
        .chain(&code.lazy_decl_reads)
    {
        push_unique(out, *sym);
    }
    for op in &code.ops {
        let idx = code
            .op_container_mutate_const_idx(op)
            .or_else(|| CompiledCode::op_callee_name_const_idx(op));
        if let Some(idx) = idx
            && let Some(ValueView::Str(name)) = code.constants.get(idx as usize).map(Value::view)
            && !code.locals.iter().any(|l| l.as_str() == name.as_str())
        {
            push_unique(out, Symbol::intern(name.as_str()));
        }
    }
    for nested in &code.closure_compiled_codes {
        collect_by_name_reads(nested, out);
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::opcode::OpCode;

    /// Every `RegisterDecl` of the top-level code of `src` is bounded, i.e.
    /// none of them forces the frame-wide env-sync fold.
    fn all_top_level_decls_bounded(src: &str) -> bool {
        let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);
        let decls: Vec<u32> = code
            .ops
            .iter()
            .filter_map(|op| match op {
                OpCode::RegisterDecl(idx) => Some(*idx),
                _ => None,
            })
            .collect();
        assert!(!decls.is_empty(), "no declaration in {src:?}");
        decls
            .iter()
            .all(|idx| code.bounded_lazy_decl_plans.contains(idx))
    }

    #[test]
    fn static_token_is_bounded() {
        assert!(all_top_level_decls_bounded(
            "grammar G { token t { a <b>+ }; token b { b } }; my $x = 1;"
        ));
    }

    #[test]
    fn interpolating_token_stays_unbounded() {
        assert!(!all_top_level_decls_bounded(
            "my $p = 'a'; grammar G { token t { <$p> } }"
        ));
    }

    #[test]
    fn computed_method_name_is_bounded() {
        assert!(all_top_level_decls_bounded(
            "my constant n = 'm'; my $y = 1; class C { method ::(n) { $y } }"
        ));
        assert!(all_top_level_decls_bounded(
            "my $y = 1; my $cn = 'K'; class ::($cn) { method m { $y } }"
        ));
    }

    #[test]
    fn hoisted_shells_are_bounded() {
        assert!(all_top_level_decls_bounded(
            "my $z = 1; say 1; class F { method m { $z } }; role R { method m { $z } }"
        ));
    }

    #[test]
    fn nested_type_in_sub_is_bounded() {
        assert!(all_top_level_decls_bounded(
            "my $v = 1; sub f { my class In { method v { $v } }; In.v }"
        ));
        assert!(all_top_level_decls_bounded(
            "my $v = 1; class O { method go { my role IR { method v { $v } }; IR.v } }"
        ));
    }
}
