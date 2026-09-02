use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{ArgSupply, AssignOp, CallArg, Expr, PhaserKind, Stmt, make_anon_sub};
use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction, OpCode};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

static STATE_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// §1.4 shadow-slot activation gate.
///
/// When active, [`Compiler::declare_local`] gives a shadowing inner-block `my $x`
/// its **own** fresh local slot (instead of sharing the outer `$x`'s slot) and
/// [`Compiler::pop_local_scope`] restores the outer binding in `local_map` on
/// scope exit, so shadowing correctness no longer relies on the runtime
/// `BlockScope` whole-`locals` restore.
///
/// **Default ON since 2026-07-12.** A fresh full toggle-ON whitelist survey
/// (1379 files) found **zero genuine regressions** — every file that fails ON
/// also fails OFF — confirming the §1.5 leaf-slot bakes (S1–S17) plus the
/// 2026-07-10 container-identity/cell work drove the §1.3 class-1 (name-keyed
/// env dual-store) breakage to zero. `MUTSU_NO_SHADOW_SLOTS` is a temporary
/// opt-out escape hatch (reverts to the old shared-slot + env-restore behavior).
/// See docs/lexical-scope-slot-campaign.md and ANALYSIS.md §1.4.
pub(crate) fn shadow_slots_active() -> bool {
    use std::sync::OnceLock;
    static ACTIVE: OnceLock<bool> = OnceLock::new();
    *ACTIVE.get_or_init(|| std::env::var_os("MUTSU_NO_SHADOW_SLOTS").is_none())
}

#[cfg(test)]
mod declaration_plan_tests {
    use super::Compiler;
    use crate::ast::Stmt;

    #[test]
    fn sub_declarations_leave_the_generic_statement_pool() {
        let (stmts, _) = crate::parse_dispatch::parse_source("sub f($x) { $x + 1 }; f(2)")
            .expect("source parses");
        let (code, compiled_fns) = Compiler::new().compile(&stmts);

        assert!(!code.sub_decl_plans.is_empty());
        let plan = code
            .sub_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "f" && !plan.compiled_routine_keys.is_empty())
            .expect("source-order f declaration plan");
        assert_eq!(plan.compiled_routine_keys.len(), 1);
        assert!(compiled_fns.contains_key(&plan.compiled_routine_keys[0]));
        assert!(
            code.stmt_pool
                .iter()
                .all(|stmt| !matches!(stmt, crate::ast::Stmt::SubDecl { .. })),
            "compiled sub declarations must not remain executable generic statements"
        );
        assert!(code.ops.iter().any(|op| matches!(
            op,
            crate::opcode::OpCode::RegisterDecl(idx)
                if matches!(
                    code.decl_plans.get(*idx as usize),
                    Some(crate::opcode::CompiledDeclPlanRef::Sub(_))
                )
        )));
    }

    #[test]
    fn nested_compilation_units_remap_colliding_routine_plan_keys() {
        let source = r#"
            sub outer-a() { multi sub inner(Int $x) { 1 }; &inner }
            sub outer-b() { multi sub inner(Int $x) { 2 }; &inner }
        "#;
        let (stmts, _) = crate::parse_dispatch::parse_source(source).expect("source parses");
        let (_code, compiled_fns) = Compiler::new().compile(&stmts);

        let inner_keys: Vec<_> = compiled_fns
            .values()
            .flat_map(|function| function.code.sub_decl_plans.iter())
            .filter(|plan| plan.name.as_str() == "inner" && !plan.compiled_routine_keys.is_empty())
            .map(|plan| plan.compiled_routine_keys[0])
            .collect();
        // Each `inner` is registered from two plans — the hoist pass's and the
        // source-order one — and both carry the same compiled routine, so the
        // two declarations account for four key-bearing plans.
        assert_eq!(inner_keys.len(), 4);
        let distinct: std::collections::HashSet<_> = inner_keys.iter().copied().collect();
        assert_eq!(distinct.len(), 2);
        for key in &distinct {
            assert!(compiled_fns.contains_key(key));
        }
    }

    #[test]
    fn type_declarations_leave_the_generic_statement_pool() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "role R { method r { 1 } }; class C does R { method c { 2 } }; C.new.c",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        assert!(!code.class_decl_plans.is_empty());
        assert!(!code.role_decl_plans.is_empty());
        assert!(code.stmt_pool.iter().all(|stmt| !matches!(
            stmt,
            crate::ast::Stmt::ClassDecl { .. } | crate::ast::Stmt::RoleDecl { .. }
        )));
        assert!(code.ops.iter().any(|op| matches!(
            op,
            crate::opcode::OpCode::RegisterDecl(idx)
                if matches!(
                    code.decl_plans.get(*idx as usize),
                    Some(crate::opcode::CompiledDeclPlanRef::Class(_))
                )
        )));
        assert!(code.ops.iter().any(|op| matches!(
            op,
            crate::opcode::OpCode::RegisterDecl(idx)
                if matches!(
                    code.decl_plans.get(*idx as usize),
                    Some(crate::opcode::CompiledDeclPlanRef::Role(_))
                )
        )));
    }

    /// ADR-0019 G2 (architectural guard): a `token`/`rule` declaration —
    /// whether at the top level (F7 slice 1) or inside a class body (F7
    /// slice 2) — never leaves an executable `Stmt::TokenDecl`/`RuleDecl`
    /// clone in the generic `stmt_pool`, mirroring the sub/class/proto
    /// guard tests above. The regex body itself stays an opaque payload on
    /// the typed plan (`CompiledTokenDeclPlan::raw_body`/`ClassBodyOp::TokenRule`'s
    /// own `raw_body`) — ADR-0009's accepted execution model, not something
    /// this guard checks.
    #[test]
    fn token_rule_declarations_leave_the_generic_statement_pool() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "token top_level { \\d+ }; class A { rule in_class { \\d+ } }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        assert!(
            code.stmt_pool.iter().all(|stmt| !matches!(
                stmt,
                crate::ast::Stmt::TokenDecl { .. } | crate::ast::Stmt::RuleDecl { .. }
            )),
            "compiled token/rule declarations must not remain executable generic statements"
        );

        // Top-level: registers via RegisterDecl(CompiledDeclPlanRef::Token).
        assert!(!code.token_decl_plans.is_empty());
        assert!(
            code.token_decl_plans
                .iter()
                .any(|plan| plan.name.as_str() == "top_level")
        );
        assert!(code.ops.iter().any(|op| matches!(
            op,
            crate::opcode::OpCode::RegisterDecl(idx)
                if matches!(
                    code.decl_plans.get(*idx as usize),
                    Some(crate::opcode::CompiledDeclPlanRef::Token(_))
                )
        )));

        // Class body: registers via ClassBodyOp::TokenRule, not Other/chunk.
        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");
        assert!(plan_a.body_plan.iter().any(|op| matches!(
            op,
            crate::opcode::ClassBodyOp::TokenRule { plan }
                if plan.name.as_str() == "in_class"
        )));
    }

    /// ADR-0019 G2 (architectural guard, comprehensive sweep): one program
    /// declaring every migrated declaration kind at once — top-level `sub`,
    /// `proto sub`/`multi sub`, `token`, `rule`, `class`, and a class body's
    /// own nested `method`/`token`/`rule` — leaves NO declaration-shaped
    /// `Stmt` in `stmt_pool` (top level, nested compiled functions, or a
    /// class body's own `ClassBodyOp::Other`/`ClassSub` raw payload). The
    /// per-kind tests above each cover one declaration kind in isolation;
    /// this test exists because that isolation could hide a kind-vs-kind
    /// interaction (e.g. a nested declaration only regressing when a
    /// sibling kind is also present) that no single-kind test would catch.
    /// `stmt_pool` itself is not asserted empty — it also carries
    /// non-declaration payloads (`gather` bodies, closures) — only that it
    /// holds none of the six migrated declaration `Stmt` variants.
    ///
    /// A **role body** is deliberately NOT swept the same way: every
    /// `RoleBodyOp::Deferred` statement (not just `token`/`rule`) keeps a
    /// raw `Stmt` by design — a role's composing package is unknown until
    /// composition (ADR-0019 D8-1/D8-2), so `Deferred` is the accepted
    /// carve-out bucket for any deferred statement kind, not a regression
    /// surface this guard should police.
    #[test]
    fn every_migrated_declaration_kind_together_leaves_the_generic_statement_pool() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "sub f($x) { $x };
             proto sub p($x) {*}; multi sub p(Int $x) { $x };
             token t1 { \\d+ };
             rule r1 { \\d+ };
             class C {
                 method cm { 2 }
                 token ct { 'y' }
                 rule cr { 'z' }
             }",
        )
        .expect("source parses");
        let (code, compiled_fns) = Compiler::new().compile(&stmts);

        let is_migrated_decl = |stmt: &Stmt| {
            matches!(
                stmt,
                Stmt::SubDecl { .. }
                    | Stmt::ProtoDecl { .. }
                    | Stmt::TokenDecl { .. }
                    | Stmt::RuleDecl { .. }
                    | Stmt::ClassDecl { .. }
                    | Stmt::RoleDecl { .. }
            )
        };
        assert!(
            code.stmt_pool.iter().all(|stmt| !is_migrated_decl(stmt)),
            "top-level stmt_pool: {:?}",
            code.stmt_pool
        );
        for function in compiled_fns.values() {
            assert!(
                function
                    .code
                    .stmt_pool
                    .iter()
                    .all(|stmt| !is_migrated_decl(stmt)),
                "nested compiled-function stmt_pool: {:?}",
                function.code.stmt_pool
            );
        }
        for plan in &code.class_decl_plans {
            for op in &plan.body_plan {
                if let crate::opcode::ClassBodyOp::Other { raw, .. }
                | crate::opcode::ClassBodyOp::ClassSub { raw, .. } = op
                {
                    assert!(
                        !is_migrated_decl(raw),
                        "class {} body op: {raw:?}",
                        plan.name
                    );
                }
            }
        }
    }

    /// ADR-0019 C8: a non-trivial proto body compiles its `{*}`-rewritten
    /// dispatch once, at declaration time, instead of leaving the VM to
    /// rewrite and OTF-compile the AST on every call.
    #[test]
    fn nontrivial_proto_declarations_compile_their_dispatch_body() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "proto sub f($x) { say 'before'; {*} }; multi sub f(Int $x) { $x }; f(1)",
        )
        .expect("source parses");
        let (code, compiled_fns) = Compiler::new().compile(&stmts);

        assert!(!code.proto_decl_plans.is_empty());
        let plan = code
            .proto_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "f")
            .expect("proto f declaration plan");
        let key = plan
            .compiled_routine_key
            .expect("non-trivial proto body must compile a routine");
        assert!(compiled_fns.contains_key(&key));
        assert!(
            code.stmt_pool
                .iter()
                .all(|stmt| !matches!(stmt, crate::ast::Stmt::ProtoDecl { .. })),
            "compiled proto declarations must not remain executable generic statements"
        );
        assert!(code.ops.iter().any(|op| matches!(
            op,
            crate::opcode::OpCode::RegisterDecl(idx)
                if matches!(
                    code.decl_plans.get(*idx as usize),
                    Some(crate::opcode::CompiledDeclPlanRef::Proto(_))
                )
        )));
    }

    /// A trivial proto (`{*}` only) dispatches implicitly and has no
    /// candidate body of its own to compile.
    #[test]
    fn trivial_proto_declarations_compile_no_dispatch_body() {
        let (stmts, _) =
            crate::parse_dispatch::parse_source("proto sub g($x) {*}; multi sub g(Int $x) { $x }")
                .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan = code
            .proto_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "g")
            .expect("proto g declaration plan");
        assert!(plan.compiled_routine_key.is_none());
    }

    /// ADR-0019 C8 scoping (2026-08-17): a `proto method`/`proto submethod`
    /// NEVER compiles a dispatch body, even with a non-trivial body, unlike a
    /// package-level proto sub above. Giving a proto method's `{*}` body its
    /// own compiled routine is unbuilt capability (`run_proto_method` still
    /// tree-walks `CompiledProtoDeclPlan::legacy_body` for every proto
    /// method call) — this is the load-bearing fact behind that scoping
    /// note's "RETAIN `legacy_body`" conclusion. If this test ever needs to
    /// change, the scoping note's conclusion needs revisiting too.
    #[test]
    fn nontrivial_proto_method_declarations_compile_no_dispatch_body() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "class C { proto method f($x) { say 'before'; {*} }; multi method f(Int $x) { $x } }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let class_plan = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "C")
            .expect("class C declaration plan");
        let chunk = class_plan
            .body_plan
            .iter()
            .find_map(|op| match op {
                crate::opcode::ClassBodyOp::ProtoMethod { chunk, .. } => chunk.as_ref(),
                _ => None,
            })
            .expect("proto method f compiled as a ClassBodyOp::ProtoMethod chunk");
        let plan = chunk
            .code
            .proto_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "f" && plan.is_method)
            .expect("proto method f declaration plan");
        assert!(
            plan.compiled_routine_key.is_none(),
            "a proto method never compiles its own dispatch body, trivial or not"
        );
    }

    /// ADR-0019 D1: a class declaration's stub-ness and `trusts` targets are
    /// precomputed at plan lowering, so registration never re-walks the body
    /// to judge them (`check_class_role_redeclaration`, `publish_class_shell`).
    #[test]
    fn class_declarations_precompute_stub_and_trusts() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "class A { trusts B; has $.x }; class B { }; class Stub { ... }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");
        assert!(!plan_a.is_stub);
        assert_eq!(
            plan_a.trusts.iter().map(|s| s.as_str()).collect::<Vec<_>>(),
            vec!["B"]
        );

        let plan_stub = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "Stub")
            .expect("class Stub declaration plan");
        assert!(plan_stub.is_stub);
        assert!(plan_stub.trusts.is_empty());
    }

    /// ADR-0019 D6-1: names a class body `my`/`state`-declares at its own
    /// top level are precomputed at plan lowering, so
    /// `persist_class_body_statics` never re-walks the raw body to derive
    /// them. `our`/`dynamic` declarations and non-`VarDecl` statements are
    /// excluded.
    #[test]
    fn class_declarations_precompute_declared_static_names() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "class A { my $x = 1; state $y = 2; our $z = 3; has $.w }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");
        let mut names: Vec<_> = plan_a
            .declared_static_names
            .iter()
            .map(|s| s.as_str())
            .collect();
        names.sort_unstable();
        assert_eq!(names, vec!["x", "y"]);
    }

    /// ADR-0019 D4-2: a bracketed `is`/`does` parent whose bracket content
    /// parsed as a clean expression list (D4-1) is precompiled into a
    /// declaration-trait-arg chunk per argument, keyed by the same
    /// concatenated parent string `parents`/`does_parents` use. A literal
    /// argument stays a `Literal` with no chunk; a non-literal argument
    /// (here, a variable reference) compiles to a `Compiled` chunk.
    #[test]
    fn class_declarations_precompute_parent_arg_chunks() {
        let (stmts, _) =
            crate::parse_dispatch::parse_source("class A is Bar[Int, $x] does Baz[42] { }")
                .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");
        let mut keys: Vec<_> = plan_a
            .parent_arg_chunks
            .iter()
            .map(|(key, _)| key.as_str())
            .collect();
        keys.sort_unstable();
        assert_eq!(keys, vec!["Bar[Int, $x]", "Baz[42]"]);

        let (_, bar_args) = plan_a
            .parent_arg_chunks
            .iter()
            .find(|(key, _)| key == "Bar[Int, $x]")
            .expect("Bar[Int, $x] chunk entry");
        assert_eq!(bar_args.len(), 2);
        assert!(matches!(
            bar_args[1],
            crate::opcode::DeclTraitArg::Compiled(_)
        ));

        let (_, baz_args) = plan_a
            .parent_arg_chunks
            .iter()
            .find(|(key, _)| key == "Baz[42]")
            .expect("Baz[42] chunk entry");
        assert_eq!(baz_args.len(), 1);
        let literal = baz_args[0].literal().expect("Baz[42] arg is a literal");
        assert!(matches!(literal.view(), crate::value::ValueView::Int(42)));
    }

    /// ADR-0019 D2a: a class declaration's own attribute names — including
    /// ones nested inside a body `sub`, and excluding class-level `our`/`my`
    /// attributes — are precomputed at plan lowering, so `run_class_body`
    /// never re-walks the body to derive them.
    #[test]
    fn class_declarations_precompute_own_attribute_names() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "class A { has $.x; has $!y; our $.z; sub f { has $.w } }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");
        let mut names: Vec<_> = plan_a
            .own_attribute_names
            .iter()
            .map(|s| s.as_str())
            .collect();
        names.sort_unstable();
        assert_eq!(names, vec!["w", "x", "y"]);
    }

    /// ADR-0019 D6-3a/b/c: `body_plan` mirrors `run_class_body`'s own
    /// flattened dispatch order one-op-per-statement (including the
    /// interstitial `Stmt::SetLine` markers the parser inserts, which
    /// classify as `Other` the same way `run_class_body`'s `_` arm treats
    /// them today), with a nested-sub `has` appended at the end (matching
    /// `own_attribute_names`'s own append order), classifies each statement
    /// kind into the right op, and compiles a standalone chunk for every
    /// arm that carries a raw statement (`Other`/`ClassSub` in D6-3b,
    /// `CodeAlias`/`ProtoMethod`/`LeavePhaser` in D6-3c).
    #[test]
    fn class_declarations_precompute_body_plan() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            r#"
            class A {
                has $.x;
                method m() { 42 }
                also does Baz;
                sub helper() { 1 }
                our &alias ::= &m;
                proto method p(|) {*}
                my $will-be-static will leave { 1 } = 1;
                sub f { has $.w }
            }
            "#,
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");

        // Independently re-derive the flattened statement count (same
        // transform `class_body_plan` applies) straight from the AST, so
        // the length check does not hardcode a count sensitive to the
        // parser's own `SetLine` insertion behavior.
        let Stmt::ClassDecl { body, .. } = stmts
            .iter()
            .find(|s| matches!(s, Stmt::ClassDecl { name, .. } if name.as_str() == "A"))
            .expect("class A declaration statement")
        else {
            unreachable!()
        };
        let mut flattened: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        fn collect_nested_has<'a>(stmts: &'a [Stmt], out: &mut Vec<&'a Stmt>) {
            for s in stmts {
                match s {
                    Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } | Stmt::HasDecl { .. } => {}
                    Stmt::SubDecl { body, .. } => {
                        for inner in body {
                            if matches!(inner, Stmt::HasDecl { .. }) {
                                out.push(inner);
                            }
                        }
                        collect_nested_has(body, out);
                    }
                    _ => {}
                }
            }
        }
        collect_nested_has(body, &mut flattened);
        assert_eq!(plan_a.body_plan.len(), flattened.len());

        // Every `Other` op (the `SetLine` markers and the `my`-lexical
        // statement, matching `declared_static_names`'s own separate
        // handling of body statics) got its own compiled chunk (D6-3b).
        use crate::opcode::ClassBodyOp;
        for op in &plan_a.body_plan {
            if let ClassBodyOp::Other { chunk, .. } = op {
                assert!(chunk.is_some(), "Other op missing a compiled chunk: {op:?}");
            }
        }

        // Filtering out `Other` ops leaves exactly the typed arms, in
        // source order, with the nested-sub `has` appended at the tail.
        let typed: Vec<&ClassBodyOp> = plan_a
            .body_plan
            .iter()
            .filter(|op| !matches!(op, ClassBodyOp::Other { .. }))
            .collect();
        assert_eq!(typed.len(), 9, "typed ops: {typed:?}");
        assert!(matches!(
            typed[0],
            ClassBodyOp::Attr { name, .. } if name.as_str() == "x"
        ));
        assert!(matches!(typed[1], ClassBodyOp::Method));
        assert!(matches!(
            typed[2],
            ClassBodyOp::Does { name, .. } if name.as_str() == "Baz"
        ));
        // `ClassSub` shares `Other`'s chunk mechanism (D6-3b).
        assert!(matches!(
            typed[3],
            ClassBodyOp::ClassSub { name, chunk: Some(_), .. } if name.as_str() == "helper"
        ));
        // `CodeAlias`/`ProtoMethod`/`LeavePhaser` compile the same way (D6-3c).
        assert!(matches!(
            typed[4],
            ClassBodyOp::CodeAlias { chunk: Some(_), .. }
        ));
        assert!(matches!(
            typed[5],
            ClassBodyOp::ProtoMethod { chunk: Some(_), .. }
        ));
        // `my $will-be-static will leave { 1 } = 1` lowers to a
        // `SyntheticBlock` of [the `VarDecl` (an `Other` op), the `will
        // leave` trait's own `Phaser { kind: Leave, .. }` statement].
        assert!(matches!(
            typed[6],
            ClassBodyOp::LeavePhaser { chunk: Some(_), .. }
        ));
        // `sub f { has $.w }` is itself a `SubDecl` (another `ClassSub`),
        // whose nested `has $.w` is the last op, appended at the tail.
        assert!(matches!(
            typed[7],
            ClassBodyOp::ClassSub { name, chunk: Some(_), .. } if name.as_str() == "f"
        ));
        assert!(matches!(
            typed[8],
            ClassBodyOp::Attr { name, .. } if name.as_str() == "w"
        ));
    }

    /// ADR-0019 F7 slice 2: `token`/`rule` declarations inside a class body
    /// classify into their own typed `ClassBodyOp::TokenRule` plan instead
    /// of falling into `Other`'s raw-`Stmt` fallback — they need no
    /// `CompiledDeclExpr` chunk (the regex body stays interpreter-executed
    /// per ADR-0009), but the registration shell (name/params/multi) is
    /// precomputed just like the top-level `RegisterDecl(Token)` path.
    #[test]
    fn class_declarations_body_plan_types_token_rule_declarations() {
        let (stmts, _) =
            crate::parse_dispatch::parse_source("class A { token t { a }; rule r { a } }")
                .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");

        use crate::opcode::ClassBodyOp;
        let token_rule_ops: Vec<&ClassBodyOp> = plan_a
            .body_plan
            .iter()
            .filter(|op| matches!(op, ClassBodyOp::TokenRule { .. }))
            .collect();
        assert_eq!(token_rule_ops.len(), 2, "ops: {token_rule_ops:?}");
        let names: Vec<String> = token_rule_ops
            .iter()
            .map(|op| {
                let ClassBodyOp::TokenRule { plan } = op else {
                    unreachable!()
                };
                plan.name.resolve()
            })
            .collect();
        assert_eq!(names, vec!["t".to_string(), "r".to_string()]);
        assert!(
            plan_a
                .body_plan
                .iter()
                .all(|op| !matches!(op, ClassBodyOp::Other { raw, .. }
                    if matches!(raw, Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. })))
        );
    }

    /// ADR-0019 D6-3d: `ClassBodyOp::LeavePhaser`'s chunk must compile the
    /// phaser's own *inner* body, not the wrapping `Stmt::Phaser` statement
    /// — `compiler/stmt.rs`'s `Stmt::Phaser { .. } => {}` catch-all arm
    /// compiles a bare (un-lowered) `PhaserKind::Leave` statement to a
    /// no-op, since LEAVE is normally driven by the enclosing `BlockScope`
    /// registering a callback rather than by direct statement compilation.
    /// Compiling the wrapper directly (the pre-fix behavior) would silently
    /// produce a dead chunk with no observable side effect.
    #[test]
    fn class_declarations_leave_phaser_chunk_compiles_inner_body() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "class A { my $will-be-static will leave { $tracker = 99 } = 1; }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_a = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "A")
            .expect("class A declaration plan");

        use crate::opcode::ClassBodyOp;
        let phaser_chunk = plan_a
            .body_plan
            .iter()
            .find_map(|op| match op {
                ClassBodyOp::LeavePhaser { chunk: Some(c), .. } => Some(c),
                _ => None,
            })
            .expect("a compiled LeavePhaser chunk");
        // The wrapper-statement compile (the pre-fix behavior) emits no
        // opcodes for `$tracker = 99` at all; compiling the inner body
        // directly must emit real assignment bytecode.
        assert!(
            !phaser_chunk.code.ops.is_empty(),
            "LeavePhaser chunk must not be empty (compiled the no-op wrapper instead of the inner body)"
        );
        assert!(
            phaser_chunk
                .code
                .constants
                .iter()
                .any(|v| v.to_string_value() == "99"),
            "LeavePhaser chunk should embed the inner body's `99` literal, ops: {:?}",
            phaser_chunk.code.ops
        );
    }

    /// ADR-0019 D6-3d: an `Other` op's chunk must qualify a bare package
    /// variable against the *declaring class's own name*, not the outer
    /// (enclosing) compiler's ambient package — `t/strict-use-and-eval.t`'s
    /// `no strict; class Foo { $foo = 42; }` regressed under the
    /// `MUTSU_DROP_LEGACY_CLASS_BODY=1` instrument until this was fixed:
    /// `Compiler::qualify_variable_name` bakes package qualification in at
    /// COMPILE time, and a body-plan chunk compiled with the wrong ambient
    /// `current_package` silently wrote a bare (unqualified) global instead
    /// of `Foo::foo`, diverging from `run_block_raw`'s registration-time
    /// compile (which qualifies against the interpreter's
    /// `current_package()`, already switched to `Foo` by then).
    #[test]
    fn class_declarations_other_chunk_qualifies_against_declaring_class() {
        let (stmts, _) =
            crate::parse_dispatch::parse_source("class Foo { $foo = 42; }").expect("parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_foo = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "Foo")
            .expect("class Foo declaration plan");

        // A `SetLine` marker statement (interstitial, parser-inserted) is
        // also classified as `Other` and compiles to an empty chunk, so
        // check every `Other` chunk rather than assuming the first is the
        // `$foo = 42;` assignment.
        use crate::opcode::ClassBodyOp;
        let other_chunks: Vec<_> = plan_foo
            .body_plan
            .iter()
            .filter_map(|op| match op {
                ClassBodyOp::Other { chunk: Some(c), .. } => Some(c),
                _ => None,
            })
            .collect();
        assert!(
            !other_chunks.is_empty(),
            "expected at least one Other chunk"
        );
        assert!(
            other_chunks.iter().any(|c| c
                .code
                .constants
                .iter()
                .any(|v| v.to_string_value() == "Foo::foo")),
            "an Other chunk should qualify `$foo` as `Foo::foo`, chunks' constants: {:?}",
            other_chunks
                .iter()
                .map(|c| &c.code.constants)
                .collect::<Vec<_>>()
        );
    }

    /// ADR-0019 D2a: a role declaration's own attribute names, `use`d module
    /// names, and body-declared type names are precomputed at plan lowering,
    /// so `walk_role_body`'s pre-scan pass never re-derives them.
    #[test]
    fn role_declarations_precompute_body_prescan() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "role R { use JSON::Fast; has $.x; my class Inner { } }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");
        assert_eq!(
            plan_r
                .own_attribute_names
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>(),
            vec!["x"]
        );
        assert_eq!(plan_r.body_used_modules, vec!["JSON::Fast".to_string()]);
        assert_eq!(plan_r.body_declared_types, vec!["Inner".to_string()]);
    }

    /// ADR-0019 D7-1/D9-1: a role declaration's stub-ness and its first
    /// our-scope violation (if any) are precomputed at plan lowering, so
    /// `register_role_decl` never re-walks the raw body to derive them.
    #[test]
    fn role_declarations_precompute_stub_and_our_scope_violation() {
        let (stmts, _) =
            crate::parse_dispatch::parse_source("role Stub { ... }; role Plain { has $.x }")
                .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_stub = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "Stub")
            .expect("role Stub declaration plan");
        assert!(plan_stub.is_stub);
        assert_eq!(plan_stub.our_scope_violation, None);

        let plan_plain = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "Plain")
            .expect("role Plain declaration plan");
        assert!(!plan_plain.is_stub);
        assert_eq!(plan_plain.our_scope_violation, None);

        let (stmts, _) =
            crate::parse_dispatch::parse_source("role R { our $x = 1 }").expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);
        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");
        assert_eq!(plan_r.our_scope_violation, Some("variable"));

        let (stmts, _) =
            crate::parse_dispatch::parse_source("role R { class C {} }").expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);
        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");
        assert_eq!(plan_r.our_scope_violation, Some("class"));

        // A `my class` inside a role is lexically scoped and allowed.
        let (stmts, _) =
            crate::parse_dispatch::parse_source("role R { my class C {} }").expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);
        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");
        assert_eq!(plan_r.our_scope_violation, None);
    }

    /// ADR-0019 D7-4: a role's `body_plan` is an ordered, typed mirror of
    /// its (single-level flattened) body, one op per statement
    /// `walk_role_body`'s dispatch loop visits — the role-side twin of
    /// D6-3a's class `body_plan`.
    #[test]
    fn role_declarations_precompute_body_plan() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            r#"
            role R does Some {
                has $.x;
                method m { 42 }
                does Baz;
                say "hi";
            }
            "#,
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");

        // Independently re-derive the flattened statement count (same
        // single-level transform `role_body_plan` applies), so the length
        // check does not hardcode a count sensitive to the parser's own
        // `SetLine` insertion behavior.
        let Stmt::RoleDecl { body, .. } = stmts
            .iter()
            .find(|s| matches!(s, Stmt::RoleDecl { name, .. } if name.as_str() == "R"))
            .expect("role R declaration statement")
        else {
            unreachable!()
        };
        let flattened: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        assert_eq!(plan_r.body_plan.len(), flattened.len());

        // Filtering out `Deferred` ops (the `SetLine` markers and the `say`
        // statement) leaves exactly the typed arms, in source order. The
        // role-header `does Some` clause is a synthetic `DoesDecl`
        // prepended to the body, so it appears before the body-level
        // `does Baz` clause even though both classify as `Parent`.
        use crate::opcode::RoleBodyOp;
        let typed: Vec<&RoleBodyOp> = plan_r
            .body_plan
            .iter()
            .filter(|op| !matches!(op, RoleBodyOp::Deferred { .. }))
            .collect();
        assert_eq!(typed.len(), 4, "typed ops: {typed:?}");
        assert!(matches!(typed[0], RoleBodyOp::Parent));
        assert!(matches!(
            typed[1],
            RoleBodyOp::Attr { name, .. } if name.as_str() == "x"
        ));
        assert!(matches!(typed[2], RoleBodyOp::Method));
        assert!(matches!(typed[3], RoleBodyOp::Parent));
    }

    /// ADR-0019 D8-1/D8-2: each deferred (non-attribute, non-method,
    /// non-`does`) role-body statement gets its own precompiled
    /// `DeferredBodyOp`, one per `RoleBodyOp::Deferred` entry in
    /// `body_plan`. Only `TypeDecl` (a nested `class`/`role`, which always
    /// registers under the role's own package regardless of composition
    /// site) gets a compiled `chunk`; `TokenRule` (composing-class package
    /// unknown until composition) and `Plain` (the ambient package at the
    /// composition call site, also unknown until composition — see
    /// `compile_role_deferred_body`'s doc comment for the
    /// `my package G { class A is Array[T] {} }` case that ruled this out)
    /// both keep `chunk: None` and fall back to `raw`. A non-`our`/
    /// non-`dynamic` `VarDecl` records its own name in `declared_vars`.
    #[test]
    fn role_declarations_precompute_deferred_body() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            r#"
            role R {
                has $.x;
                method m { 42 }
                my $y = 1;
                token t { a }
                my class Inner { }
                say "hi";
            }
            "#,
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let plan_r = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");

        use crate::opcode::RoleBodyOp;
        // ADR-0019 D8-2: `deferred_body_ops` additionally filters out
        // `SetLine` markers (and the `__mutsu_stub_die`/`__mutsu_stub_warn`
        // stub markers) that `body_plan`'s `Deferred` catch-all still
        // matches — `walk_role_body`'s own runtime dispatch never defers
        // either, so keeping them out of `deferred_body_ops` is what makes
        // it empty for a method-only role body, matching runtime dispatch
        // (see `compile_role_deferred_body`'s doc comment). Count only the
        // "real" deferred statements here.
        let deferred_count = plan_r
            .body_plan
            .iter()
            .filter(|op| {
                matches!(
                    op,
                    RoleBodyOp::Deferred { raw, .. }
                        if !matches!(raw.as_ref(), Stmt::SetLine(_))
                )
            })
            .count();
        assert_eq!(plan_r.deferred_body_ops.len(), deferred_count);

        use crate::opcode::DeferredBodyOpKind;
        let var_op = plan_r
            .deferred_body_ops
            .iter()
            .find(|op| matches!(&op.raw, Stmt::VarDecl { name, .. } if name == "y"))
            .expect("the `my $y = 1` deferred op");
        assert_eq!(var_op.kind, DeferredBodyOpKind::Plain);
        assert!(var_op.chunk.is_none());
        assert_eq!(
            var_op
                .declared_vars
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>(),
            vec!["y"]
        );

        let token_op = plan_r
            .deferred_body_ops
            .iter()
            .find(|op| matches!(&op.raw, Stmt::TokenDecl { .. }))
            .expect("the `token t { a }` deferred op");
        assert_eq!(token_op.kind, DeferredBodyOpKind::TokenRule);
        assert!(token_op.chunk.is_none());
        assert!(token_op.declared_vars.is_empty());

        let class_op = plan_r
            .deferred_body_ops
            .iter()
            .find(|op| matches!(&op.raw, Stmt::ClassDecl { .. }))
            .expect("the `my class Inner { }` deferred op");
        assert_eq!(class_op.kind, DeferredBodyOpKind::TypeDecl);
        assert!(class_op.chunk.is_some());
        assert!(class_op.declared_vars.is_empty());

        // Every other deferred op (the `SetLine` markers and the `say`
        // statement) is `Plain` and keeps `chunk: None`.
        for op in &plan_r.deferred_body_ops {
            if op.kind == DeferredBodyOpKind::Plain {
                assert!(op.chunk.is_none(), "unexpected chunk for {op:?}");
            }
        }
    }

    /// ADR-0019 G2 (architectural guard): `legacy_body` was dropped from
    /// `CompiledSubDeclPlan`/`CompiledClassDeclPlan`/`CompiledRoleDeclPlan` by
    /// C6/D6/D9 — reintroducing it on any of the three would be a regression
    /// to the tree-walk era this ADR exists to retire. `CompiledProtoDeclPlan`
    /// is the one deliberate, permanent exception (C8's own scoping note:
    /// `call_proto_function`'s interpreter fallback and
    /// `vm_resolve_trivial_proto_candidate` both still need it), so this test
    /// asserts the field is present there and absent everywhere else, rather
    /// than banning the name outright. `Vec<T>`'s derived `Debug` never omits
    /// a field, so a struct-literal field boundary check on the formatted
    /// output ("legacy_body:", not a bare substring match — `alternate_body`
    /// or similar would otherwise false-negative) is a real regression guard,
    /// not a tautology: it fails to compile only if the field is renamed, and
    /// fails at runtime if it is reintroduced under this exact name.
    #[test]
    fn legacy_body_survives_only_on_the_proto_decl_plan() {
        let (stmts, _) = crate::parse_dispatch::parse_source(
            "sub f($x) { $x };
             proto sub p($x) {*}; multi sub p(Int $x) { $x };
             role R { method rm { 1 } };
             class C does R { method cm { 2 } }",
        )
        .expect("source parses");
        let (code, _) = Compiler::new().compile(&stmts);

        let has_legacy_body_field = |debug: &str| debug.contains("legacy_body:");

        let sub_plan = code
            .sub_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "f")
            .expect("sub f declaration plan");
        assert!(
            !has_legacy_body_field(&format!("{sub_plan:?}")),
            "CompiledSubDeclPlan must not carry legacy_body"
        );

        let class_plan = code
            .class_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "C")
            .expect("class C declaration plan");
        assert!(
            !has_legacy_body_field(&format!("{class_plan:?}")),
            "CompiledClassDeclPlan must not carry legacy_body"
        );

        let role_plan = code
            .role_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "R")
            .expect("role R declaration plan");
        assert!(
            !has_legacy_body_field(&format!("{role_plan:?}")),
            "CompiledRoleDeclPlan must not carry legacy_body"
        );

        let proto_plan = code
            .proto_decl_plans
            .iter()
            .find(|plan| plan.name.as_str() == "p")
            .expect("proto sub p declaration plan");
        assert!(
            has_legacy_body_field(&format!("{proto_plan:?}")),
            "CompiledProtoDeclPlan is the one accepted permanent legacy_body exception (C8)"
        );
    }

    /// ADR-0019 G2 (architectural guard): dispatch must resolve every method
    /// call through the canonical `Registry`/`MethodEntry` table, never
    /// through a per-`ClassDef` method mirror — F4c removed `ClassDef::methods`
    /// for exactly this reason. This is a compile-time guard: `ClassDef`
    /// (`runtime/decl_types.rs`) derives `Default`, so a stray reintroduction
    /// of a `methods` field would not break any existing construction site
    /// (it would just default silently, unlike the plan structs above, which
    /// have no `Default` impl). A field-boundary Debug check on a real,
    /// non-default-constructed `ClassDef` is the guard: it fails to compile
    /// if the field is renamed, and fails at runtime if `methods` (exactly —
    /// not `native_methods`, which is unrelated and legitimately present) is
    /// reintroduced.
    #[test]
    fn class_def_carries_no_method_mirror_field() {
        let debug = format!("{:?}", crate::runtime::ClassDef::default());
        assert!(
            !debug.contains(" methods:") && !debug.starts_with("methods:"),
            "ClassDef must not carry a `methods` field — dispatch reads only \
             from the canonical Registry/MethodEntry table (ADR-0019 F4c): {debug}"
        );
    }
}
mod const_fold;
mod decl_plan;
mod expr;
mod expr_binary;
mod expr_block;
mod expr_call;
mod expr_closure;
mod expr_data;
mod expr_helpers;
mod expr_method;
mod expr_ops;
mod expr_postfix;
mod expr_unary;
mod helpers;
mod helpers_ast_utils;
mod helpers_block_inline;
mod helpers_call_args;
mod helpers_control_flow;
mod helpers_do_expr;
mod helpers_dynamic;
mod helpers_method_body;
pub(crate) mod helpers_ops;
mod helpers_phasers;
mod helpers_placeholder_binds;
mod helpers_stmt_analysis;
mod helpers_sub_body;
pub(crate) mod lex_scope;
mod nqp_forms;
mod stmt;

#[derive(Clone)]
pub(crate) struct Compiler {
    code: CompiledCode,
    local_map: HashMap<String, u32>,
    /// Lexical scope stack for local-slot allocation (§1.4 groundwork). Pushed and
    /// popped at every block boundary (`push_dynamic_scope_lexical` /
    /// `pop_dynamic_scope_lexical`); each frame records the names declared in that
    /// scope via [`declare_local`], the single declaration entry point.
    ///
    /// This is intentionally INERT today — `declare_local` still resolves slots
    /// like `alloc_local` (a nested `my $x` shares the outer `$x`'s slot), so there
    /// is no behavior change and no duplicate names in `code.locals`. The map value
    /// (`Option<u32>` = the pre-declaration slot to restore on scope exit) is the
    /// shape the future fix needs; it is left `None` for now.
    ///
    /// Giving a shadow its own fresh slot — the actual collision fix — is deferred
    /// because it breaks the VM's by-name runtime slot resolution and env↔slot
    /// coherence, and must be done as one campaign with §1.5 (remove name-based
    /// slot resolution) and §1.3 (collapse the dual store). See ANALYSIS.md §1.4.
    /// Frame 0 is the compilation-unit / routine top level and is never popped.
    local_scopes: Vec<HashMap<String, Option<u32>>>,
    /// The ENCLOSING compilation's scope chain (outermost first), for compilers
    /// that are compiling a nested body. `local_scopes` stops at the routine /
    /// closure boundary because slot allocation does, but the *lexical* chain does
    /// not — and `$::($name)::x` has to be answered against the whole thing, since
    /// it learns its target name too late to be answered any other way. Empty for a
    /// compilation unit's own compiler. See [`lex_scope::LexScopeChain`].
    enclosing_scopes: Vec<lex_scope::ScopeFrame>,
    /// Index (within `local_scopes`) of this compilation UNIT's outermost scope —
    /// the scope `UNIT::` names. 0 for a file's own compiler; an `EVAL`'d unit
    /// (`mark_as_eval_unit`) pushes an empty wrapper frame first so that
    /// `OUTER::` from its mainline lands on nothing, which also means its true
    /// mainline is one frame in — so `UNIT::` must stop there, not at the wrapper.
    unit_root_scope: usize,
    /// True when this compiler is compiling a routine or closure BODY rather
    /// than a compilation unit's mainline. A `sub`/`multi` declared there is
    /// lexical to that body, so it may shadow a same-named routine belonging to
    /// a different scope — mutsu's routine registry is keyed by package alone,
    /// and without this marker a `sub f` in one sub body made a `multi f` in a
    /// *sibling* body an X::Redeclaration.
    in_lexical_scope: bool,
    /// Routine names this body declares more than once in a way that conflicts
    /// (not every declaration is `multi`). Those must NOT be marked lexical:
    /// the conflict is inside this one scope, so it is a genuine
    /// X::Redeclaration and the runtime check has to keep seeing it. Computed
    /// once from the body's own statements, so it does not depend on how many
    /// times a declaration is compiled (the hoist pass compiles each twice).
    lexical_dup_routines: HashSet<String>,
    /// Declaration plans emitted by the hoist pass, as
    /// `(routine name, site fingerprint, decl plan index)`.
    ///
    /// A sub declaration is registered twice: once from the hoist pass at the
    /// top of its block, and once in source order. Only the source-order site
    /// compiles the body, so only it can record `compiled_routine_keys` — which
    /// leaves the hoisted registration installing a routine with no bytecode.
    /// For a single sub the later install simply replaces it, but a `multi`
    /// candidate is *appended* to its name's candidate set, so the bytecode-less
    /// hoisted candidate survives and answers calls. The source-order site
    /// therefore back-fills its keys into the matching hoisted plan (same name
    /// and same `sub_registration_fingerprint`, i.e. the same declaration).
    hoisted_sub_plans: Vec<(crate::symbol::Symbol, u64, u32)>,
    /// Track type constraints for local variables (for compile-time literal checks).
    local_types: HashMap<String, String>,
    compiled_functions: CompiledFns,
    current_package: String,
    /// True when compiling inside a `unit module`/`unit class`/`unit role`
    /// body. Used to pre-qualify class/role declarations with the package
    /// prefix at compile time, since the runtime does not get a
    /// PackageScope opcode for unit declarations.
    pub(crate) in_unit_package: bool,
    /// When `Some`, every `my`/`state` variable declaration compiled while the
    /// stack-top frame is active is recorded here. A scope-isolating do-block
    /// expression (e.g. a string-interpolation `{...}`) uses this to learn the
    /// names it declares — including ones nested in expressions (`(state $a)++`)
    /// or shadowing an outer same-name — so its isolating exit reverts exactly
    /// those while letting OUTER-variable mutations persist. A nested closure
    /// compiles in a fresh `Compiler` (own scope) so it never pollutes this.
    pub(crate) block_decl_tracker: Vec<Vec<String>>,
    /// Expression declarations inside a synthesized WhateverCode belong to the
    /// surrounding source block and therefore store through its captured slot.
    promoted_expr_decl_names: HashSet<String>,
    /// The kind of package (`module`/`package`/`grammar`) whose body is
    /// currently being compiled, or `None` in the mainline. Used to raise
    /// X::Attribute::Package when a `has` attribute is declared in a
    /// module/package body (which cannot hold attributes).
    pub(crate) current_package_kind: Option<crate::ast::PackageKind>,
    /// The enclosing package name before closure mangling. Used for `$?PACKAGE`
    /// so that methods inside a class report the class name, not the internal
    /// closure package name.
    pub(crate) enclosing_package: Option<String>,
    tmp_counter: usize,
    dynamic_scope_all: bool,
    dynamic_scope_names: Option<std::collections::HashSet<String>>,
    /// User routines named like container listops that are visible in the
    /// current compiler lexical scope. Seeded by the sub-hoist pass so a call
    /// before its textual declaration resolves like Rakudo.
    user_listop_shadows: std::collections::HashSet<String>,
    /// Track dynamic variable accesses (names starting with '*') for the
    /// X::Dynamic::Postdeclaration check. Scoped to the CURRENT lexical block
    /// only (reset by `push_dynamic_scope_lexical`, restored by
    /// `pop_dynamic_scope_lexical` — same lifecycle as `my_vars_current_scope`):
    /// Raku only flags a `my $*x := ...` declaration as illegal when an EARLIER
    /// read of `$*x` appears directly in that SAME block, not when the read was
    /// in an enclosing or sibling scope (see `LexicalScopeSnapshot`).
    accessed_dynamic_vars: std::collections::HashSet<String>,
    /// Number of enclosing `for`-loop blocks between the code currently being
    /// compiled and the enclosing routine. A `for` block is a distinct call
    /// frame in Raku (`callframe(0)` inside a `for` body is the block, the
    /// routine is one level up), so `callframe`/`caller` call sites capture this
    /// depth (as the hidden `__callframe_blocks` arg) and the runtime offsets the
    /// requested level by it. Reset to 0 for each nested routine (nested subs get
    /// a fresh `Compiler`). Only `for` blocks are counted: `while`/`loop`/`if`
    /// bodies are elided frames in Rakudo and unreliable to model.
    pub(crate) callframe_block_depth: u32,
    /// Whether we are compiling inside a routine (sub/method). `return` outside
    /// a routine must throw X::ControlFlow::Return instead of normal return.
    pub(crate) is_routine: bool,
    /// Whether the enclosing lexical scope contains a routine (sub/method).
    /// Used to decide whether `return` in a non-routine block should perform
    /// a non-local return (via CX::Return) or throw X::ControlFlow::Return.
    pub(crate) lexically_in_routine: bool,
    /// ADR-0037 §2.3: set only for an EVAL unit whose `context => $ctx` named
    /// a routine that had already exited the dynamic call stack when the
    /// `EVAL` ran (decided once, at EVAL entry, by `builtin_eval`). Only
    /// meaningful together with `is_routine == false` — it makes the emitted
    /// `ReturnFromNonRoutine` throw `X::ControlFlow::Return` with
    /// `out-of-dynamic-scope` set and rakudo's fuller wording, instead of the
    /// plain "no routine at all" message, matching raku's dead-context
    /// probe (ADR-0037 §1.1(c)).
    pub(crate) eval_context_dead_routine: bool,
    /// Whether we are directly compiling the body statements of a plain
    /// `Stmt::Block` that emits `OpCode::BlockScope` (see the `Stmt::Block`
    /// arm in `stmt.rs`). That opcode snapshots `env` before the body and
    /// restores it after, dropping any new env key the body introduced — so
    /// a `my TYPE $x` declaration compiled while this flag is set can safely
    /// use the env-only `SetVarTypeScoped` opcode instead of the both-store
    /// `SetVarType`, exactly like inside a routine
    /// (`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`, issue
    /// 2 "Mainline blocks"). Set/restored narrowly around that one branch —
    /// the other `Stmt::Block` branches (implicit try, phaser scope,
    /// `LetBlock`, import scope) do not perform this env restore, so a `my`
    /// directly inside one of those keeps using the unscoped opcode; a
    /// plain block nested inside any of them still gets its own
    /// `BlockScope` and sets this flag again for its own body.
    ///
    /// Also set for the two other body kinds whose env the VM restores on exit,
    /// via `compile_block_local_branch` / `compile_scope_restored_loop_body`:
    /// an `if`/`unless`/`else` branch that declares a block-local `my`
    /// (`OpCode::BlockLocalScope`) and every loop body
    /// (`while`/`until`/C-style `loop`/`repeat`/`for`, whose opcodes bracket the
    /// body with `push_loop_local_scope`/`pop_loop_local_scope`). Both restore a
    /// declaration's `__mutsu_type::` metadata to its pre-body state — see
    /// `Interpreter::save_type_meta_for_scope_exit` — which is exactly the
    /// guarantee this flag stands for. Inside a routine these bodies were
    /// already scoped through `is_routine`/`lexically_in_routine`; the flag is
    /// what extends it to the same shapes at mainline.
    pub(crate) lexically_in_block: bool,
    /// Whether the enclosing routine is a `method` (or submethod). A method
    /// always carries an implicit `*%_` / `*@_` slurpy, so the legacy argument
    /// variables `%_` / `@_` are valid lexicals throughout its body — including
    /// inside a nested `do {}` block that does not itself take a signature. A
    /// plain `sub` has no such implicit slurpy, so `%_` there only works as a
    /// per-block placeholder and may not appear in a nested signature-less block.
    pub(crate) lexically_in_method: bool,
    /// True when the enclosing routine's own signature declares a parameter
    /// spelled `$self` — an explicit invocant (`method m($self: $n)`,
    /// `method symbol(::?CLASS $self: ...)`) or an ordinary parameter
    /// (`sub ($self)`, `-> $self, $x`). A parser-synthesized *anonymous*
    /// invocant (`method () {}`, `method (Foo:D:)`, `method (::?CLASS:)`) is
    /// excluded: it is named `self` only because that is the invocant's env key,
    /// and it declares no lexical.
    ///
    /// Such a parameter is named `self` in `ParamDef`, so it binds the plain env
    /// key `"self"`. The parser gives every `$`-sigiled `self` the reserved
    /// lexical key [`crate::env::LEX_SELF`] (ADR-0061); inside such a routine the
    /// compiler maps it back to `"self"`, so the body reads its own parameter
    /// rather than an unrelated outer lexical. Inherited by nested blocks and
    /// closures, exactly like the lexical scope it describes.
    pub(crate) self_is_signature_param: bool,
    /// When true, the current VarDecl is from a `:=` bind declaration.
    bind_vardecl: bool,
    /// When true, the current VarDecl is a `:=` bind whose target is a
    /// SIGILLESS term (`my \a := ...`, or a sigilless target of a
    /// list-destructuring bind). Set by the enclosing `SyntheticBlock`, which is
    /// the only place that knows -- the trailing `MarkSigilless` compiles after
    /// the declaration. One-shot, like [`Self::bind_vardecl`].
    sigilless_bind_vardecl: bool,
    /// True while compiling the RHS of a sigilless `:=` bind, so the terminal
    /// index emits `IndexAutovivifyLazyTerminal { sigilless: true }` and leaves
    /// an immutable `List`'s scalar element unpromoted (and hence immutable).
    sigilless_bind_terminal: bool,
    /// True only while compiling the statement operand of `do`. `WheneverScope`
    /// reads this to leave its Tap on the ordinary value stack.
    do_stmt_yields_value: bool,
    /// When true, Index expressions should emit IndexAutovivify instead of
    /// Index.  Set only during scalar `:=` bind VarDecl compilation so that
    /// `my $b := %h<foo><baz>` creates a HashEntryRef.
    scalar_bind_autovivify: bool,
    /// When true (alongside `scalar_bind_autovivify`), the next Index compiled is
    /// the TERMINAL element of the bind RHS (outermost subscript whose value is
    /// bound). A terminal index promotes even a container-valued (Array/Hash) leaf
    /// to a cell. Cleared while compiling the inner `target` so only the outermost
    /// index is terminal.
    bind_terminal: bool,
    /// ADR-0059: when true we are compiling the operand of `return-rw`, i.e. an
    /// expression that denotes a *storage location* the routine is handing to
    /// its caller, not a value. Every subscript on the path to that location is
    /// therefore part of the lvalue chain and must yield the element's
    /// container: the top-level one (so `return-rw c<a>` returns the element
    /// cell) and any that appears as an ARGUMENT of a nested call
    /// (`return-rw in(container{@steps[0]}, @steps[1..*])` — Crane's recursive
    /// descent, where the argument must alias the real sub-container so the
    /// eventual leaf write lands in the caller's structure and each level
    /// autovivifies). Distinct from `scalar_bind_autovivify`, which must NOT
    /// change call-argument compilation (a call nested in a `:=` RHS keeps the
    /// ordinary `is rw` writeback machinery — see `bind_target_direct`).
    /// Cleared while compiling a closure body, which is a fresh routine.
    rw_return_operand: bool,
    /// ADR-0059 Slice 2: this compiler is compiling the body of an `is rw` /
    /// `is raw` routine, whose *bare tail expression* (no `return-rw`) denotes
    /// the storage location the routine hands its caller. The tail is then
    /// compiled exactly like a `return-rw` operand (`compile_return_rw_arg`),
    /// so `sub f() is rw { %h<k> }` returns the element's container and
    /// `sub f() is rw { $x }` returns `$x`'s cell. Set per routine body by
    /// `compile_sub_body_with_deprecation` / `compile_method_body` /
    /// the `is rw` anonymous-sub paths; a nested closure gets its own
    /// compiler with the flag off.
    rw_tail: bool,
    /// When true, the *immediate* upcoming `compile_call_arg` call compiles a
    /// `:=` bind/rebind target (`my $x := @a[$i]`), not a genuine function-call
    /// argument. `compile_call_arg` reads this once at entry and clears it
    /// before any nested compilation, so a call nested inside the bind RHS
    /// (`my $x := f(@a[$i])`) still sees `false` for its own arguments and
    /// keeps the normal `is rw` writeback machinery. Guards against reusing the
    /// call-argument `is rw` Index writeback temps (`__mutsu_index_rw_*`) for a
    /// bind: those temps are compile-time-fixed global names, and inside a loop
    /// body the same bind statement re-executes every iteration, so the
    /// call-argument writeback's "write through an existing ContainerRef"
    /// semantics corrupt the *previous* iteration's bound cell instead of
    /// storing a fresh one (see the `lock.t` array-corruption investigation).
    bind_target_direct: bool,
    /// ADR-0021 I2/I3: when true, the *immediate* upcoming `Expr::Binary{
    /// FatArrow}` compile (`compile_expr_binary`) mints the named-argument
    /// flavour (`OpCode::MakeNamedArg`) instead of the data-default
    /// (`OpCode::MakePair`). Set only around compiling a call/method
    /// argument that IS, at its top level, a bareword-keyed fat-arrow or
    /// colonpair (`is_named_arg_expr`'s `Binary` arm — the same shape
    /// `ContainerizePair` boundary erasure is skipped for, since the
    /// call-site syntax already marks it named). `compile_expr_binary` reads
    /// and clears this once at entry, before recursing into the key/value
    /// sub-expressions, so a Pair nested in the value (`f(a => (b => 1))`)
    /// does not inherit it — only the outermost, genuinely-named pair does.
    mint_named_pair: bool,
    /// Variables declared as `constant` (no Scalar container).
    constant_vars: std::collections::HashSet<String>,
    /// Scalar variables `:=`-bound to a non-itemized value (no Scalar
    /// container), so `for $x` iterates the bound value's elements rather than
    /// treating `$x` as a single item. Populated when the bind RHS produces a
    /// non-itemized value (a list/constructor/method-call/container-var, or
    /// another already-non-itemized bound scalar). A bind to a plain itemized
    /// scalar (`my $x := $itemized`) inherits the item container and is NOT
    /// recorded. See `normalize_for_iterable`.
    noncontainer_bound_vars: std::collections::HashSet<String>,
    /// Subset of `constant_vars` whose declaring lexical block is still open.
    /// Constants are `our`-scoped (installed in the package), so once their
    /// declaring block has exited, their stale local slot must not be reused —
    /// such bare-word accesses fall back to GetBareWord (package/global lookup).
    constant_vars_in_scope: std::collections::HashSet<String>,
    /// Constants declared in the *current* lexical block only (reset on block
    /// entry). Declaring the same constant twice in one block is an
    /// X::Redeclaration; a shadowing declaration in an inner block is allowed.
    constant_vars_current_scope: std::collections::HashSet<String>,
    /// Plain `my` variable names declared in the *current* lexical block only
    /// (reset on block entry). Redeclaring an existing same-scope `my` variable
    /// *without* an explicit initializer (`my $f` / `my Int $f`) is a no-op in
    /// Raku — the variable keeps its current value (only a "Redeclaration of
    /// symbol" warning is emitted). A redeclaration *with* an initializer
    /// (`my $f = 10`) does run the assignment. Tracked here so the VarDecl
    /// compiler can suppress the reset for the bare-redeclaration case.
    my_vars_current_scope: std::collections::HashSet<String>,
    /// Names of fully-defined classes/roles declared in the *current* lexical
    /// block only (reset on block entry). Declaring the same class name twice in
    /// one scope is an X::Redeclaration ("Redeclaration of symbol 'A'"); a stub
    /// (`class A {...}`) followed by its real definition is NOT a redeclaration,
    /// and a same-named class in an inner block shadows rather than redeclares.
    class_names_current_scope: std::collections::HashSet<String>,
    /// Names of constants declared in an *enclosing* compiler (i.e. visible at a
    /// nested closure's definition point). Propagated into child closure
    /// compilers (which otherwise start with empty constant state). Used ONLY to
    /// detect that a `constant X` inside the closure *shadows* an outer constant
    /// — a shadowing constant is purely lexical and must not clobber the outer
    /// constant's shared package store. This set is deliberately NOT consulted
    /// during bare-word resolution (that stays driven by `constant_vars_in_scope`
    /// + `local_map`), so it cannot turn an outer-constant read into a GetLocal.
    outer_constant_names: std::collections::HashSet<String>,
    /// Local names that are sigilless bindings (declared with `my \Foo = ...`
    /// or as a sigilless parameter).  BareWord resolution only uses GetLocal
    /// for names in this set; `$`-sigiled variables must not shadow type names.
    sigilless_locals: std::collections::HashSet<String>,
    /// Sigilless-binding names declared in an ENCLOSING compilation (the parent
    /// sub/block's `sigilless_locals`, transitively). A BareWord naming one of
    /// these — but not a local of the current frame — is a genuine lexical
    /// capture, so it is compiled as a by-name global read (recorded as a free
    /// variable and captured) rather than a plain `GetBareWord` that would
    /// degrade to the literal name string once the creating frame is gone
    /// (escaping closure / `.^add_method`).
    enclosing_sigilless: std::collections::HashSet<String>,
    /// Local names visible in enclosing compiled frames. A same-named local
    /// declaration in this compiler shadows a captured binding even though the
    /// outer binding has no slot in this chunk.
    enclosing_local_names: std::collections::HashSet<String>,
    /// Placeholder params (`^p` caret-form) an interpret-path caller has
    /// already bound in env before re-compiling this body — see
    /// `seed_prebound_placeholders`.
    pub(super) prebound_placeholder_params: std::collections::HashSet<String>,
    /// Set true immediately before compiling a *synthesized* `Stmt::Block`
    /// (an if/while/loop/control branch body the compiler wraps at compile time,
    /// not a genuine source `{ ... }`). The `Stmt::Block` arm consumes it to
    /// decide whether the resulting scope is a backtrace-visible callframe.
    synthetic_block_body: bool,
    /// Address of the `Stmt::Block` body a statement modifier (or a `while`)
    /// supplies its own value to — ADR-0048 D3/D6. See
    /// `Compiler::note_construct_body_block`, which records it, and
    /// `is_construct_body_block`, which the `Stmt::Block` arm consults to skip
    /// its zero-argument arity check for exactly that node.
    construct_body_block: Option<usize>,
    /// Set true immediately before the ONE NEXT `push_dynamic_scope_lexical`
    /// call recursively invoked to inline a `Stmt::SyntheticBlock`'s body (the
    /// parser's wrapper for a `:=`-bind declaration, e.g. `my $x := expr` ->
    /// `[MarkReadonly, VarDecl]`) via `compile_block_inline` in block-final
    /// (tail) position. A `SyntheticBlock` is never a genuine lexical scope —
    /// its direct (non-tail) dispatch (`Stmt::SyntheticBlock` in
    /// `compile_stmt`) inlines its statements with NO push/pop at all — so
    /// this flag tells that one push not to reset `accessed_dynamic_vars`:
    /// otherwise a dynamic-var read earlier in the SAME enclosing block would
    /// be lost right before the wrapped declaration's own
    /// X::Dynamic::Postdeclaration check runs, wrongly treating a genuine
    /// same-block postdeclaration as legal just because it happened to be the
    /// block's last statement. Consumed (reset to false) by
    /// `push_dynamic_scope_lexical`.
    next_dynamic_scope_inline_transparent: bool,
    /// Set true immediately before compiling a loop body that is a sole source
    /// `{ ... }` block (the `{ ... } for @xs` statement-modifier form). The
    /// `Stmt::Block` arm consumes it to skip the block's per-execution
    /// `ResetStateLocals`: that block is the loop's body, cloned once per loop
    /// statement, so its `state` persists across iterations — see
    /// `loop_body_is_sole_block`.
    suppress_loop_block_state_reset: bool,
    /// Set by the `Stmt::Block` arm just before calling `compile_try` for a
    /// genuine bare block that carries a `CATCH`/`CONTROL`. `compile_try`
    /// consumes it so the emitted `TryCatch` is marked as a bare-block
    /// callframe. Other `compile_try` callers (e.g. `try { }`) leave it false.
    next_try_is_bare_block: bool,
    /// Line of the `Stmt::SetLine` marker last seen (the line attached to the ops
    /// emitted since; also the definition line of a block/sub compiled here).
    last_source_line: Option<i64>,
    /// How many `BEGIN <expr>` sites with the same (package, line, body) identity
    /// this compilation has already emitted. Disambiguates two textually
    /// identical BEGINs on one line so they keep separate memo cells; see
    /// `begin_site_id`.
    begin_site_seq: std::collections::HashMap<u64, u32>,
    /// Pending writebacks for Index expressions passed to function calls.
    /// After the call returns, if the `is rw` parameter was written to,
    /// we need to write the temp variable value back to the original hash/array slot.
    /// Each entry is (original Index Expr, temp variable name).
    pub(super) pending_index_rw_writebacks: Vec<(Expr, String, String)>,
    /// The current distribution context for $?DISTRIBUTION.
    pub(crate) current_distribution: Option<Value>,
    /// True while compiling a sub-expression whose VALUE is stored/returned/bound
    /// (an *escaping position*): assignment or `:=` RHS, `return`/`fail` operand,
    /// block/routine tail, or a literal element. A closure created while this is
    /// set has its value escape the creating frame, forcing a shared `ContainerRef`
    /// cell for the captured-and-mutated locals it closes over (escape analysis;
    /// see `CompiledCode::closure_escapes`). Default false = the conservative
    /// non-escaping (immediately-invoked) classification, so call arguments and
    /// control-construct blocks never over-box (the #2746 perf guard).
    escaping_position: bool,
    /// Subset of `escaping_position`: the closure is handed to a thread
    /// (`start { ... }`, `Thread.start`, `Promise.start`). See
    /// `CompiledCode::thread_escaping`.
    thread_escaping_position: bool,
    /// True while compiling the body of an `our`-scoped named sub. An `our sub` is
    /// installed into the package registry and stays callable after its declaring
    /// block exits, so the lexicals it reads/writes must be boxed into shared cells
    /// and persisted (see `CompiledCode::escaping_our_sub_captures`). Read in
    /// `compile_sub_body_with_deprecation` when contributing the sub's captures to
    /// the enclosing scope.
    pub(crate) compiling_our_sub: bool,
    /// True only for the outermost compilation unit (the mainline / a top-level
    /// EVAL). Used to detect placeholder variables (`$^x`, `@_`, ...) that appear
    /// outside any sub or block -> X::Placeholder::Mainline.
    pub(crate) is_mainline: bool,
    /// When true, a `key => $var` Pair must NOT capture `$var`'s container.
    /// Set while compiling call arguments: a named argument's value is passed
    /// to the callee by the call's binding rules (and decontainerized for plain
    /// params / attribute stores), so capturing the container there breaks code
    /// that reads the bound value without deref (e.g. `.new(prefix => $dir)`).
    /// Standalone Pair literals (`my $p = (k => $v)`) still capture for
    /// write-through (S02:1704).
    suppress_pair_capture: bool,
    /// When set, an `ArrayLiteral` does NOT box its scalar-variable elements into
    /// aliasing `ContainerRef` cells (the List container-aliasing behavior). Set
    /// while compiling a `for`-loop's synthetic single-element iterable wrap
    /// (`for $a` -> `ArrayLiteral([$a])`): the loop already handles `is rw`
    /// write-back through its own `TagContainerRef` mechanism, so aliasing here
    /// would write the shared cell back into `$a` and create a self-referential
    /// `ContainerRef` cycle (infinite loop on the next read).
    suppress_list_var_alias: bool,
    /// Set by `Stmt::Expr` for exactly one call, when the top-level statement
    /// expression is directly a list-assignment (`($a,$b) = ...;`, no `my`)
    /// whose own rvalue is unconditionally discarded by the `SinkPop` that
    /// follows. Consumed (and cleared) immediately inside the list-assignment
    /// handling in `expr_call.rs`, before any nested/chained sub-expression
    /// (e.g. the RHS, or a chained `($a,$b) = (($c,$d) = ...)`) is compiled —
    /// only the outermost call's own synthetic result-list construction is
    /// affected. When true, that construction is skipped entirely (a cheap
    /// `Nil` placeholder is pushed instead of the real
    /// `WrapVarRef`+`MakeArray` sequence): building the real aliased list
    /// would box each target back into a shared `ContainerRef` cell and write
    /// it into the flat `env`, purely to be popped and discarded — and worse,
    /// that stale cell corrupts the NEXT reader of the same (flat, unscoped)
    /// env key, since env is a single namespace shared across frames. See
    /// `todo/deep/sunk-list-reassign-leaks-containerref-into-shared-env.md`.
    sunk_list_assign_result: bool,
    /// Constant-folding state (ADR-0006 §2.1), shared with every child compiler
    /// of this compilation unit so an operator declaration found while compiling
    /// a sub body disables folding for the whole unit.
    fold_ctx: std::sync::Arc<const_fold::FoldCtx>,
    /// True only for the compiler that owns `fold_ctx` (the unit-level one).
    /// Child compilers inherit the Arc and must not trigger the refold pass.
    fold_root: bool,
    /// Compile-time values of in-scope `constant`s whose initializer is itself a
    /// constant scalar (ADR-0006 §2.2). Reads of these compile to `LoadConst`
    /// instead of a package lookup, and an `if`/`unless` on one resolves its
    /// branch at compile time. Follows `constant_vars_in_scope`'s lifecycle: a
    /// constant leaving its declaring block stops being inlined (it is then an
    /// `our`-scoped package symbol again).
    constant_values: HashMap<String, Value>,
    /// Same, for constants declared in an *enclosing* compiler — a sub body must
    /// still inline the file-level `constant DEBUG` it reads.
    outer_constant_values: HashMap<String, Value>,
}

/// How [`Compiler::compile_phaser_block_scope`] should dispose of a
/// phaser-bearing block's trailing value.
pub(super) enum PhaserBlockResult {
    /// Leave the value on the VM stack for the caller to consume (an
    /// expression-context block, e.g. a `do { ... }` block or an `if`/`given`
    /// used as an expression).
    Push,
    /// Route the value through the topic register as this compiled unit's
    /// own implicit return value. Only safe when `stmts` is a fresh call
    /// frame's own body (a routine's compiled body) — that frame's topic
    /// register is its own, so this cannot leak into a caller's `$_`.
    ReturnViaTopic,
    /// Discard the value entirely, without touching the topic. Used for
    /// same-frame statement contexts (a bare `{ ... }` statement, an
    /// `if`/`given` body compiled as a statement) where the enclosing
    /// scope's live `$_` (e.g. a `given`'s topicalized value) must survive
    /// the body's own trailing statement.
    Discard,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
            local_map: HashMap::new(),
            // Frame 0 = compilation-unit / routine top level; never popped.
            local_scopes: vec![HashMap::new()],
            enclosing_scopes: Vec::new(),
            unit_root_scope: 0,
            in_lexical_scope: false,
            lexical_dup_routines: HashSet::new(),
            hoisted_sub_plans: Vec::new(),
            local_types: HashMap::new(),
            compiled_functions: CompiledFns::default(),
            current_package: "GLOBAL".to_string(),
            in_unit_package: false,
            block_decl_tracker: Vec::new(),
            promoted_expr_decl_names: HashSet::new(),
            current_package_kind: None,
            enclosing_package: None,
            tmp_counter: 0,
            dynamic_scope_all: false,
            dynamic_scope_names: None,
            user_listop_shadows: std::collections::HashSet::new(),
            accessed_dynamic_vars: std::collections::HashSet::new(),
            callframe_block_depth: 0,
            is_routine: false,
            lexically_in_routine: false,
            eval_context_dead_routine: false,
            lexically_in_block: false,
            lexically_in_method: false,
            self_is_signature_param: false,
            bind_vardecl: false,
            sigilless_bind_vardecl: false,
            sigilless_bind_terminal: false,
            do_stmt_yields_value: false,
            scalar_bind_autovivify: false,
            bind_terminal: false,
            rw_return_operand: false,
            rw_tail: false,
            bind_target_direct: false,
            mint_named_pair: false,
            constant_vars: std::collections::HashSet::new(),
            noncontainer_bound_vars: std::collections::HashSet::new(),
            constant_vars_in_scope: std::collections::HashSet::new(),
            constant_vars_current_scope: std::collections::HashSet::new(),
            my_vars_current_scope: std::collections::HashSet::new(),
            class_names_current_scope: std::collections::HashSet::new(),
            outer_constant_names: std::collections::HashSet::new(),
            sigilless_locals: std::collections::HashSet::new(),
            enclosing_sigilless: std::collections::HashSet::new(),
            enclosing_local_names: std::collections::HashSet::new(),
            prebound_placeholder_params: std::collections::HashSet::new(),
            last_source_line: None,
            begin_site_seq: std::collections::HashMap::new(),
            pending_index_rw_writebacks: Vec::new(),
            current_distribution: None,
            escaping_position: false,
            thread_escaping_position: false,
            compiling_our_sub: false,
            is_mainline: false,
            suppress_pair_capture: false,
            suppress_list_var_alias: false,
            sunk_list_assign_result: false,
            synthetic_block_body: false,
            construct_body_block: None,
            next_dynamic_scope_inline_transparent: false,
            suppress_loop_block_state_reset: false,
            next_try_is_bare_block: false,
            fold_ctx: std::sync::Arc::new(const_fold::FoldCtx::enabled()),
            fold_root: true,
            constant_values: HashMap::new(),
            outer_constant_values: HashMap::new(),
        }
    }

    /// Run `f` with the pair-capture-suppression flag set, restoring the previous
    /// value afterward. Used to mark call-argument position, where `key => $var`
    /// must pass the value (not a write-through container) to the callee.
    pub(super) fn with_suppress_pair_capture<R>(
        &mut self,
        suppress: bool,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let saved = self.suppress_pair_capture;
        self.suppress_pair_capture = suppress;
        let r = f(self);
        self.suppress_pair_capture = saved;
        r
    }

    /// Run `f` with the escaping-position flag set to `escaping`, restoring the
    /// previous value afterward. Used to mark which syntactic positions cause a
    /// closure created within them to escape its frame (see `escaping_position`).
    /// ADR-0059 Slice 2: mark this compiler as compiling an `is rw`/`is raw`
    /// routine body, whose bare tail compiles to the container it denotes.
    /// Used by the interpreter's carrier recompile of a `SubData` body, which
    /// builds its own fresh `Compiler` (`compile_block_value_opts`).
    pub(crate) fn set_rw_tail(&mut self, rw_tail: bool) {
        self.rw_tail = rw_tail;
    }

    pub(super) fn with_escape<R>(&mut self, escaping: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = self.escaping_position;
        self.escaping_position = escaping;
        let r = f(self);
        self.escaping_position = saved;
        r
    }

    /// Run `f` with the thread-escaping flag set (see
    /// `CompiledCode::thread_escaping`). Always a subset of `with_escape`.
    pub(super) fn with_thread_escape<R>(
        &mut self,
        thread_escaping: bool,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let saved = self.thread_escaping_position;
        self.thread_escaping_position = thread_escaping;
        let r = f(self);
        self.thread_escaping_position = saved;
        r
    }

    pub(crate) fn set_current_package(&mut self, package: String) {
        self.current_package = package;
    }

    /// The real (non-synthetic) package a `RegisterDecl`/`RegisterSub` op
    /// compiled from this code will see as the interpreter's runtime
    /// `current_package()`. Mirrors `qualified_class_decl_name`'s shared rule
    /// (ADR-0019 D3-8d): inside a synthetic STATE-SCOPE pseudo-package
    /// (`current_package` containing `::&`, assigned to every closure/sub
    /// body purely for `state`-variable key uniqueness — see
    /// `compile_sub_body`/`compile_closure_body`), `current_package` does NOT
    /// track the runtime package at all — a bare block/closure/sub body never
    /// itself pushes its own mangled name as the interpreter's current
    /// package (only an explicit `class`/`package`/`module`/`unit` bracketing
    /// does, always setting `current_package` directly to the real name).
    /// `self.enclosing_package` (captured before the state-scope override,
    /// propagated unchanged through arbitrarily deep closure/sub nesting) IS
    /// the runtime package in that case.
    ///
    /// A `sub` declared directly inside a closure/block body used the
    /// synthetic name uncorrected as the package component of its
    /// `compiled_fns` key (`compile_sub_body_with_deprecation`), which no
    /// runtime lookup that reconstructs candidate keys from the ACTUAL
    /// runtime package (`Interpreter::bare_name_packages`,
    /// `find_compiled_function`) could ever match — forcing every call to
    /// fall through the slow resolution ladder instead of the cached
    /// compiled-function fast path
    /// (`news/2026-08/nested-sub-in-block-otf-recompile-fixed.md`).
    pub(crate) fn runtime_current_package(&self) -> &str {
        if self.current_package.contains("::&") {
            self.enclosing_package
                .as_deref()
                .unwrap_or(&self.current_package)
        } else {
            &self.current_package
        }
    }

    pub(crate) fn qualify_package_name(&self, name: &str) -> String {
        // `GLOBAL` is the implicit root namespace: `package GLOBAL::X::Y` declares
        // `X::Y` absolutely, regardless of the enclosing package. Strip the leading
        // `GLOBAL::` and do NOT prepend the current package (otherwise a nested
        // `package GLOBAL::X::DBIish` inside `unit class DBIish` would wrongly
        // become `DBIish::X::DBIish`).
        if let Some(absolute) = name.strip_prefix("GLOBAL::") {
            return absolute.to_string();
        }
        if self.current_package == "GLOBAL" || self.current_package.contains("::&") {
            name.to_string()
        } else {
            format!("{}::{}", self.current_package, name)
        }
    }

    fn is_simple_var_expr(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
        )
    }

    pub(crate) fn qualify_variable_name(&self, name: &str) -> String {
        if self.current_package.contains("::&") {
            // Sub/method state scopes use package-like names (e.g. GLOBAL::&foo/1)
            // that should not be used to qualify runtime variable access.
            return name.to_string();
        }
        if self.current_package == "GLOBAL" || name.contains("::") {
            return name.to_string();
        }
        if name.is_empty() {
            return name.to_string();
        }
        let first = name.chars().next().unwrap();
        if matches!(first, '_' | '/' | '!' | '?' | '*' | '.' | '=') {
            return name.to_string();
        }
        // Positional capture variables ($0, $1, ...) are never qualified
        if first.is_ascii_digit() && name.chars().all(|c| c.is_ascii_digit()) {
            return name.to_string();
        }
        if let Some(sigil) = name.chars().next()
            && matches!(sigil, '$' | '@' | '%' | '&')
            && name.len() > 1
        {
            // A twigil immediately after the sigil marks a non-package variable:
            // dynamic (`@*ARGS`, `$*OUT`), compile-time (`$?FILE`), attribute
            // (`$!x`, `$.y`), etc. These must NOT be package-qualified (e.g. an
            // `@*ARGS = ...` inside `package Zef::CLI` must stay `@*ARGS`, not
            // become `@Zef::CLI::*ARGS`). Mirrors the sigilless twigil check above.
            let twigil = name[1..].chars().next();
            if matches!(twigil, Some('_' | '/' | '!' | '?' | '*' | '.' | '=')) {
                return name.to_string();
            }
            return format!("{sigil}{}::{}", self.current_package, &name[1..]);
        }
        format!("{}::{}", self.current_package, name)
    }

    /// Like [`Self::qualify_variable_name`], but for an `our`-DECLARATION's own
    /// storage key specifically (`OpCode::DeclareOurScalar`'s `qualified_idx`,
    /// and the equivalent two-store `our` sequence) — NOT for general bareword
    /// resolution.
    ///
    /// Sub/method/closure bodies compile with `current_package` overwritten by a
    /// synthetic state-scope pseudo-package (e.g. `Pkg::&foo/1`) purely for
    /// `state`-variable key uniqueness (`compile_sub_body`/`compile_closure_body`).
    /// `qualify_variable_name` deliberately bails out to the BARE name whenever it
    /// sees that pseudo-package, because most of its callers use the bare-name
    /// fallback as a GetGlobal lookup for a free/captured lexical that lives in
    /// `env` under its bare name (not a real package variable) — qualifying those
    /// against `enclosing_package` would misdirect every closure-captured-variable
    /// read to a nonexistent package-qualified key
    /// (`t/qualified-sub-captured-var-writeback-coherence.t` and ~80 other files
    /// regressed when this was tried as a change to `qualify_variable_name`
    /// itself).
    ///
    /// An `our` DECLARATION is different: it is unambiguously a package variable,
    /// and `self.enclosing_package` — captured before the state-scope override and
    /// propagated unchanged through arbitrarily deep sub/method/closure nesting —
    /// IS the real declaring package (mirrors `Compiler::runtime_current_package`).
    /// Qualifying the declaration's own storage key against that (instead of the
    /// bare name) is what makes `our $x = ...;` inside ANY sub/method/closure/regex-
    /// token body actually write through to `$Pkg::x`, not just a same-named local.
    pub(crate) fn qualify_our_variable_name(&self, name: &str) -> String {
        if self.current_package.contains("::&") {
            let pkg = self
                .enclosing_package
                .as_deref()
                .unwrap_or(&self.current_package);
            if pkg == "GLOBAL" || name.contains("::") || name.is_empty() {
                return name.to_string();
            }
            let first = name.chars().next().unwrap();
            if matches!(first, '_' | '/' | '!' | '?' | '*' | '.' | '=')
                || (first.is_ascii_digit() && name.chars().all(|c| c.is_ascii_digit()))
            {
                return name.to_string();
            }
            if let Some(sigil) = name.chars().next()
                && matches!(sigil, '$' | '@' | '%' | '&')
                && name.len() > 1
            {
                let twigil = name[1..].chars().next();
                if matches!(twigil, Some('_' | '/' | '!' | '?' | '*' | '.' | '=')) {
                    return name.to_string();
                }
                return format!("{sigil}{pkg}::{}", &name[1..]);
            }
            return format!("{pkg}::{name}");
        }
        self.qualify_variable_name(name)
    }

    /// Record a `my`/`state` declaration name for the innermost active
    /// scope-isolation tracker (see `block_decl_tracker`). No-op when no
    /// scope-isolating do-block is being compiled.
    pub(crate) fn record_block_decl(&mut self, name: &str) {
        if let Some(top) = self.block_decl_tracker.last_mut() {
            top.push(name.to_string());
        }
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        if let Some(&slot) = self.local_map.get(name) {
            return slot;
        }
        self.alloc_fresh_local(name)
    }

    /// Allocate a BRAND-NEW local slot for `name`, unconditionally (unlike
    /// [`alloc_local`], which reuses an existing same-named slot). `local_map` is
    /// repointed at the new slot. The §1.4 shadow-allocation primitive: a nested
    /// `my $x` shadowing an active-ancestor `$x` gets its own slot.
    fn alloc_fresh_local(&mut self, name: &str) -> u32 {
        let slot = self.code.locals.len() as u32;
        self.code.locals.push(name.to_string());
        self.code
            .plain_locals
            .push(Self::is_plain_lexical_name(name));
        self.local_map.insert(name.to_string(), slot);
        slot
    }

    /// See [`CompiledCode::plain_locals`]. Scalars are stored sigil-less
    /// (`my $x` -> `"x"`), so a plain lexical is a name with no sigil, twigil,
    /// qualifier or attribute marker of any kind.
    pub(crate) fn is_plain_lexical_name(name: &str) -> bool {
        !name.is_empty()
            && !name.starts_with(['$', '@', '%', '&', '.', '!', '^', '*'])
            && name != "_"
            && !name.contains("::")
            && !name.starts_with("__mutsu_")
            && !name.starts_with("__ANON")
    }

    /// Store a finalized closure body in this frame's `closure_compiled_codes`,
    /// first baking the CREATING frame's compile-time slot for each of the
    /// closure's free variables / upvalues (`local_map` at this emit point) into
    /// `free_var_parent_slots` / `upvalue_parent_slots`. Under
    /// `MUTSU_SHADOW_SLOTS` a name can occupy several creator slots, so the
    /// runtime capture paths must use the emit-point slot instead of an
    /// `rposition` name search; with the gate off the baked data is inert
    /// (§1.3 closure-capture slot bake). Scalar free vars are stored sigil-less
    /// ("x") and `@`/`%`/`&` keep their sigil — the same convention `local_map`
    /// uses, so a direct lookup lines up.
    pub(super) fn add_closure_code_baked(&mut self, mut compiled: CompiledCode, esc: bool) -> u32 {
        // OR, don't overwrite: `compute_free_vars` may already have set the flag
        // transitively from a nested thread-escaping closure.
        compiled.thread_escaping = compiled.thread_escaping || self.thread_escaping_position;
        compiled.free_var_parent_slots = compiled
            .free_var_syms
            .iter()
            .map(|sym| sym.with_str(|s| self.local_map.get(s).copied()))
            .collect();
        compiled.upvalue_parent_slots = compiled
            .upvalue_syms
            .iter()
            .map(|sym| sym.with_str(|s| self.local_map.get(s).copied()))
            .collect();
        // ADR-0032 D2: bubble this closure's container-capture edges (D1-
        // recorded during its own compile) to whichever ancestor frame owns
        // each name, as a decl-site boxing request. This is the same
        // attachment-time bubbling `compile_named_sub_body` and
        // `compile_method_body` perform for their own nested-code kind.
        if !compiled.container_ref_capture_syms.is_empty() {
            let syms = compiled.container_ref_capture_syms.clone();
            self.bubble_container_ref_capture_syms(&syms);
        }
        self.code.add_closure_code(compiled, esc)
    }

    /// ADR-0032 D2: propagate a nested compiled code's container-capture
    /// edges (`container_ref_capture_syms`, populated by D1 in
    /// [`Self::emit_wrap_var_ref`]) to the frame that actually owns each
    /// name. If `self` (the frame the nested code was just attached to)
    /// declares the name as a local, record its slot as a decl-site boxing
    /// request in `needs_cell_ref_capture_slots` — `exec_set_local_op` boxes
    /// that slot into a shared `ContainerRef` cell at its declaration
    /// (`box_decl_local_cell`). Otherwise the name is free in `self` too, so
    /// republish it into `self`'s own `container_ref_capture_syms`: the
    /// request keeps bubbling to whichever ancestor frame is two or more
    /// levels up (probe `L`/`Z4` in ADR-0032 §1.4) — the same transitive
    /// shape `named_sub_captures` / `needs_cell_named_sub_free` bubbling
    /// already uses. Slot-addressed on the Half-A side (never name-addressed)
    /// so a same-named sibling-block `my` is never mistakenly boxed
    /// (`t/list-alias-shadowed-name.t`).
    pub(super) fn bubble_container_ref_capture_syms(&mut self, child_syms: &[Symbol]) {
        for sym in child_syms {
            let owner_slot = sym.with_str(|s| self.local_map.get(s).copied());
            if let Some(parent_slot) = owner_slot {
                if !self
                    .code
                    .needs_cell_ref_capture_slots
                    .contains(&parent_slot)
                {
                    self.code.needs_cell_ref_capture_slots.push(parent_slot);
                }
            } else if !self.code.container_ref_capture_syms.contains(sym) {
                self.code.container_ref_capture_syms.push(*sym);
            }
        }
    }

    /// The full lexical scope chain visible right here, outermost first: the
    /// enclosing compilation's scopes followed by this one's.
    fn full_scope_chain(&self) -> Vec<lex_scope::ScopeFrame> {
        self.enclosing_scopes
            .iter()
            .chain(self.local_scopes.iter())
            .cloned()
            .collect()
    }

    /// Whether a `my &name` (or `&name := …`) binding is declared in a lexical
    /// scope that is still ACTIVE at this point of the compilation.
    ///
    /// Raku's rule is that such a binding shadows any package/registry routine
    /// of the same name, so a bare-name call must reach the binding. Asking
    /// `local_map` instead would be wrong: it is monotonic, so it keeps names
    /// left behind by already-popped SIBLING blocks and would capture a call
    /// that is no longer in the binding's scope.
    pub(crate) fn amp_binding_in_active_scope(&self, name: &str) -> bool {
        if name.contains("::") {
            return false;
        }
        let key = format!("&{name}");
        self.enclosing_scopes
            .iter()
            .chain(self.local_scopes.iter())
            .any(|frame| frame.contains_key(&key))
    }

    /// Hand a nested body's compiler the chain it is being compiled inside, so a
    /// symbolic deref in that body still sees the enclosing scopes.
    /// Seed the enclosing-sigilless set from an interpret-path caller (the multi
    /// / user-sub fallback), which compiles a routine body with a fresh compiler
    /// that has no signature context. Without this, a nested closure in the body
    /// would compile a bare reference to a `\thing` parameter as a bareword and
    /// lose the capture. Public so `Interpreter::compile_block_value_opts` can
    /// call it. No-op for the common empty case.
    pub(crate) fn seed_enclosing_sigilless(&mut self, names: &[String]) {
        self.enclosing_sigilless.extend(names.iter().cloned());
    }

    /// Seed the placeholder parameters an interpret-path caller has already
    /// BOUND (in env) before re-compiling a block body with a fresh compiler
    /// (`call_sub_value` → `eval_block_value`). The body of
    /// `{ 0 <= $^p <= 5 }` reaches that fresh compiler as bare statements —
    /// the chained-comparison desugar wraps them in a compiler-generated
    /// DoBlock, whose stray-placeholder check would otherwise die on `$^p`
    /// even though the closure's own signature bound it. Stored caret-form
    /// (`^p`), sigil stripped, matching `collect_unattached_placeholders`.
    pub(crate) fn seed_prebound_placeholders(&mut self, params: &[String]) {
        for p in params {
            let bare = p.trim_start_matches(['$', '@', '%', '&']);
            if bare.starts_with('^') {
                self.prebound_placeholder_params.insert(bare.to_string());
            }
        }
    }

    fn inherit_enclosing_scopes(&self, sub: &mut Compiler) {
        sub.enclosing_scopes = self.full_scope_chain();
        // Hand down every sigilless binding visible here (this frame's own plus
        // the ones it inherited) so a nested closure recognizes a bare reference
        // to an enclosing `\thing` / `my \x` as a lexical capture, not a bareword.
        sub.enclosing_sigilless
            .extend(self.sigilless_locals.iter().cloned());
        sub.enclosing_sigilless
            .extend(self.enclosing_sigilless.iter().cloned());
        sub.enclosing_local_names
            .extend(self.local_map.keys().cloned());
        sub.enclosing_local_names
            .extend(self.enclosing_local_names.iter().cloned());
    }

    /// Bake the scope chain visible right here into the code chunk, and return
    /// the index the reading opcode carries.
    ///
    /// Only the indirect spelling `$::($name)::x` needs this — see
    /// [`lex_scope::LexScopeChain`]. It is emitted per symbolic-deref site, which
    /// is a construct that appears a handful of times in a program at most.
    fn bake_lex_scope_chain(&mut self) -> u32 {
        let chain = lex_scope::LexScopeChain::new(
            self.full_scope_chain(),
            self.local_map.clone(),
            self.unit_root_index(),
            self.in_immediate_block(),
        );
        self.code.add_lex_scope_chain(chain)
    }

    /// Record the compiler-authoritative positional-parameter → local-slot map
    /// into `code.param_local_slots`, so the VM's `precompute_param_local_slots`
    /// need not re-resolve parameter names by searching `locals` (§1.5).
    ///
    /// Must be called immediately after the parameter `alloc_local` loops and
    /// BEFORE the body is compiled: at that point `local_map[name]` is exactly the
    /// parameter's slot. (Once §1.4 gives a shadowing body `my $x` its own slot,
    /// `local_map` for that name would move; recording here captures the parameter
    /// binding slot unambiguously.) The order and filtering mirror
    /// `CompiledFunction::precompute_param_local_slots`: positional `param_defs`
    /// (skipping `named`), or `params` when `param_defs` is empty; a name with no
    /// allocated slot (e.g. an anonymous `$`) is skipped.
    fn record_param_local_slots(&mut self, params: &[String], param_defs: &[crate::ast::ParamDef]) {
        let mut slots: Vec<u32> = Vec::new();
        if !param_defs.is_empty() {
            for pd in param_defs {
                if pd.named {
                    continue;
                }
                if let Some(&slot) = self.local_map.get(&pd.name) {
                    slots.push(slot);
                }
            }
        } else {
            for param in params {
                if let Some(&slot) = self.local_map.get(param) {
                    slots.push(slot);
                }
            }
        }
        self.code.param_local_slots = slots;
    }

    /// Designated entry point for a genuine `my`/`state`/`our` DECLARATION.
    ///
    /// **Groundwork, currently behavior-preserving.** It resolves the slot exactly
    /// like [`alloc_local`] (get-or-create), so a nested-block `my $x` still shares
    /// the outer `$x`'s slot — today's shadowing correctness continues to rely on
    /// the runtime env restore (`BlockScope`). The only new work is recording the
    /// declared name in the innermost [`local_scopes`] frame.
    ///
    /// The real §1.4 fix — giving a shadow its own fresh slot and restoring the
    /// outer binding on scope exit — is deliberately NOT done here: it produces
    /// duplicate names in `code.locals`, which the VM's ~40 by-name runtime slot
    /// resolvers (`find_local_slot`, the RMW writeback chokepoint, `SmartMatchExpr`,
    /// `:=`-bind, the env↔slot coherence sweeps, …) cannot disambiguate. Landing it
    /// soundly requires removing name-based runtime slot resolution (§1.5) together
    /// with the env↔slot dual store (§1.3). This scaffolding — the per-block
    /// push/pop plumbing and the single declaration entry point — is the substrate
    /// that campaign builds on. See ANALYSIS.md §1.4.
    fn declare_local(&mut self, name: &str) -> u32 {
        if !shadow_slots_active() {
            // DEFAULT build: behavior-preserving. Resolve like `alloc_local`
            // (get-or-create by name); a nested `my $x` shares the outer slot.
            let slot = self.alloc_local(name);
            if let Some(frame) = self.local_scopes.last_mut() {
                frame.entry(name.to_string()).or_insert(None);
            }
            return slot;
        }
        // §1.4 shadow-slot activation (gated by `MUTSU_SHADOW_SLOTS`).
        //
        // - Same-scope redeclaration (`my $x; my $x`) reuses the existing slot.
        // - A name declared in an ACTIVE ENCLOSING (ancestor) scope is a genuine
        //   shadow: give it a fresh slot (a second `code.locals` entry with the
        //   same name) and record the outer slot to restore on scope exit.
        // - Otherwise (a first declaration, OR a name left in the monotonic
        //   `local_map` only by an already-popped SIBLING block) allocate normally
        //   via `alloc_local` — reusing the sibling's slot. This is the crux: a
        //   shadow must be gated on an active ancestor frame, NOT on mere presence
        //   in `local_map`. `local_map` retains popped-sibling names so the runtime
        //   out-of-scope machinery keeps working; treating such a leaked name as a
        //   shadow would mint a spurious duplicate `code.locals` entry that
        //   corrupts every by-name (`position`/`rposition`) writeback resolver
        //   (`\($a)` write-through, rw-arg, undefine, …) reading the wrong slot.
        let in_current_scope = self
            .local_scopes
            .last()
            .is_some_and(|f| f.contains_key(name));
        if in_current_scope {
            return *self
                .local_map
                .get(name)
                .expect("current-scope declaration must be in local_map");
        }
        let is_ancestor_shadow = {
            let n = self.local_scopes.len();
            n >= 2
                && self.local_scopes[..n - 1]
                    .iter()
                    .any(|f| f.contains_key(name))
        };
        let prev = self.local_map.get(name).copied();
        let slot = if is_ancestor_shadow {
            self.alloc_fresh_local(name)
        } else {
            self.alloc_local(name)
        };
        if let Some(frame) = self.local_scopes.last_mut() {
            // Only a genuine ancestor shadow needs the outer slot restored on exit.
            frame.insert(
                name.to_string(),
                if is_ancestor_shadow {
                    prev
                } else if prev.is_none() && self.enclosing_local_names.contains(name) {
                    // Cross-frame shadow: no outer slot exists in this chunk.
                    // u32::MAX is a compile-time-only sentinel telling scope exit
                    // to remove the dead child mapping and resume GetGlobal.
                    Some(u32::MAX)
                } else {
                    None
                },
            );
        }
        slot
    }

    /// Declaration entry point for a routine/block PARAMETER.
    ///
    /// Slot allocation is plain [`alloc_local`], exactly as before; the added work
    /// is recording the name in the innermost [`local_scopes`] frame, because a
    /// parameter *is* a declaration of its routine's scope. `OUTER::` resolution
    /// asks that question directly ("does the target scope declare this name?"),
    /// and a signature binding must answer yes: `sub f($p) { { $OUTER::p } }` sees
    /// 42. Plain [`alloc_local`] deliberately does NOT record, since it is also how
    /// a *free* variable and compiler temporaries get a slot, and a free variable
    /// mentioned in a body is not a declaration of it (raku: `my $y = 7;
    /// sub f { say $y; { say $OUTER::y } }` prints 7 then Nil).
    fn declare_param(&mut self, name: &str) -> u32 {
        let slot = self.alloc_local(name);
        if let Some(frame) = self.local_scopes.last_mut() {
            frame.entry(name.to_string()).or_insert(None);
        }
        slot
    }

    /// True when a signature declares a parameter the *source* spelled `$self` —
    /// an explicit invocant or an ordinary positional, but not a synthesized
    /// anonymous invocant. Such a `ParamDef` is named `self` and therefore binds
    /// the plain env key `"self"`; see [`Compiler::self_is_signature_param`],
    /// [`crate::ast::ParamDef::declares_self_lexical`] and ADR-0061.
    pub(crate) fn signature_declares_self(
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
    ) -> bool {
        crate::ast::signature_declares_self_lexical(param_defs)
            // The legacy binding path only: a single pointy-block parameter
            // (`-> $self { }`) arrives as a bare name with no `ParamDef` at all.
            // Whenever `param_defs` IS populated it is authoritative — a method
            // literal (`method () { ... }`) carries `params = ["self"]` for its
            // synthesized invocant, which declares no lexical.
            || (param_defs.is_empty() && crate::ast::param_names_declare_self_lexical(params))
    }

    /// Resolve the reserved `$self` lexical key ([`crate::env::LEX_SELF`]) for
    /// the scope being compiled: inside a routine whose own signature declares a
    /// `$self` parameter it names that parameter, which binds `"self"`.
    /// Every other name passes through unchanged.
    pub(crate) fn resolve_self_lexical<'a>(&self, name: &'a str) -> &'a str {
        if self.self_is_signature_param && name == crate::env::LEX_SELF {
            "self"
        } else {
            name
        }
    }

    /// Emit a read of `bare` from the scope `depth` levels out (`$OUTER::x`,
    /// `OUTER::<$x>`, and their `OUTER::OUTER::` chains).
    fn emit_outer_var_access(&mut self, bare: String, depth: usize) {
        let res = lex_scope::resolve_outer(&self.full_scope_chain(), &self.local_map, &bare, depth);
        self.emit_outer_resolution(bare, res);
    }

    /// Emit a read of `bare` via `OUTERS::` ("Symbols in any outer lexical scope").
    fn emit_outers_var_access(&mut self, bare: String) {
        let res = lex_scope::resolve_outers(&self.full_scope_chain(), &self.local_map, &bare);
        self.emit_outer_resolution(bare, res);
    }

    /// Emit a read of `bare` via `UNIT::` (the compilation unit's outermost
    /// lexical scope): `$UNIT::x`, `UNIT::<$x>`.
    fn emit_unit_var_access(&mut self, bare: String) {
        let res = lex_scope::resolve_unit(
            &self.full_scope_chain(),
            &self.local_map,
            &bare,
            self.unit_root_index(),
        );
        self.emit_outer_resolution(bare, res);
    }

    /// Emit the read [`lex_scope`] resolved to. A scope that does not declare the
    /// name is settled here as a constant Nil rather than handed to the runtime:
    /// `OUTER` names exactly one scope, and "does that scope declare it?" is a
    /// lexical question the VM has no sound way to re-ask (see
    /// [`lex_scope::LexScopeChain`]).
    fn emit_outer_resolution(&mut self, bare: String, res: lex_scope::OuterResolution) {
        match res {
            lex_scope::OuterResolution::NotDeclared => {
                let idx = self.code.add_constant(Value::NIL);
                self.code.emit(OpCode::LoadConst(idx));
            }
            lex_scope::OuterResolution::Read { depth, slot } => {
                let name_idx = self.code.add_constant(Value::str(bare));
                self.code.emit(OpCode::GetOuterVar {
                    name_idx,
                    depth: depth as u32,
                    slot,
                });
            }
        }
    }

    /// Compile this unit as an `EVAL`'d compilation unit's mainline.
    ///
    /// Rakudo does not splice an EVAL'd unit straight into the invoking scope: it
    /// runs the compiled unit behind wrapper scopes that hold no user lexicals, so
    /// `OUTER::` from EVAL'd mainline code finds nothing (`EVAL 'OUTER::.keys'` is
    /// empty, and `my $y = 7; { say EVAL(q{OUTER::<$y>}) }` is Nil even though `$y`
    /// is right there in the invoking block). Model that by giving the unit an
    /// empty enclosing scope frame: the unit's own declarations then land one frame
    /// in, and an `OUTER::` reaching past them lands on the empty wrapper -- Nil,
    /// exactly as raku reports. Plain (unqualified) lookups are unaffected: they
    /// resolve through the runtime env, which EVAL still shares with its caller, so
    /// `my $z = 3; { say EVAL(q{$z}) }` still prints 3.
    ///
    /// This is the lexical-axis twin of [`Interpreter::push_eval_caller_frames`],
    /// which already models the same layout on the caller axis for `CALLER::`.
    pub(crate) fn mark_as_eval_unit(&mut self) {
        self.push_local_scope();
        // The EVAL'd unit's declarations now land in this freshly-pushed frame,
        // so it — not the empty wrapper below it — is the unit's `UNIT::` root.
        self.unit_root_scope = self.local_scopes.len() - 1;
    }

    /// The index, in the full (enclosing ++ local) scope chain, of this
    /// compilation unit's outermost scope — the scope `UNIT::` resolves against.
    fn unit_root_index(&self) -> usize {
        self.enclosing_scopes.len() + self.unit_root_scope
    }

    /// True when the site currently being compiled sits inside an *immediate*
    /// block (a bare block / `if` / `for` / `while` body run in place), i.e. at
    /// least one lexical scope has been pushed above this compilation's own
    /// routine/closure/unit boundary. Such a block never pushes a runtime call
    /// frame, and in Raku its dynamic caller IS its lexical parent — so a
    /// `CALLER::` here resolves lexically (`OUTER::`) rather than against the
    /// runtime call stack. `unit_root_scope` accounts for an `EVAL` wrapper frame
    /// so an EVAL'd mainline (no block of its own) is not mistaken for one.
    fn in_immediate_block(&self) -> bool {
        self.local_scopes.len() > self.unit_root_scope + 1
    }

    /// Emit a `CALLER::` read that resolves lexically (immediate-block context):
    /// `OUTER::`-style slot resolution plus the `CALLER::` dynamic-ness check.
    /// A target scope that does not declare the name is a quiet Nil, matching both
    /// `OUTER::` and raku's absent-`CALLER::` behavior.
    fn emit_caller_outer_var_access(&mut self, bare: String, depth: usize) {
        let res = lex_scope::resolve_outer(&self.full_scope_chain(), &self.local_map, &bare, depth);
        match res {
            lex_scope::OuterResolution::NotDeclared => {
                let idx = self.code.add_constant(Value::NIL);
                self.code.emit(OpCode::LoadConst(idx));
            }
            lex_scope::OuterResolution::Read { depth, slot } => {
                let name_idx = self.code.add_constant(Value::str(bare));
                self.code.emit(OpCode::GetCallerOuterVar {
                    name_idx,
                    depth: depth as u32,
                    slot,
                });
            }
        }
    }

    /// Enter a nested lexical scope for local-slot allocation. Paired with
    /// [`pop_local_scope`]; driven by the block-boundary hooks
    /// (`push_dynamic_scope_lexical`/`pop_dynamic_scope_lexical`).
    fn push_local_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    /// Leave the innermost lexical scope. Behavior-preserving today: it only drops
    /// the scope frame. When the §1.4/§1.5/§1.3 campaign gives shadows distinct
    /// slots, this is where a `Some(prev)` entry will restore the outer binding in
    /// `local_map` (see [`declare_local`]).
    fn pop_local_scope(&mut self) {
        let Some(frame) = self.local_scopes.pop() else {
            return;
        };
        if !shadow_slots_active() {
            // DEFAULT build: behavior-preserving no-op (frame already dropped).
            return;
        }
        // §1.4 shadow-slot activation: restore the outer binding for every name
        // this scope shadowed. A `None` entry was a first declaration in this
        // scope; leave its slot in `local_map` (monotonic, matching the default
        // build) so a later reference still resolves to a slot and the runtime
        // `block_declared_vars` machinery keeps enforcing out-of-scope errors.
        for (name, prev) in frame {
            if let Some(outer_slot) = prev {
                if outer_slot == u32::MAX {
                    self.local_map.remove(&name);
                } else {
                    self.local_map.insert(name, outer_slot);
                }
            }
        }
    }

    fn emit_set_named_var(&mut self, name: &str) {
        let name = self.resolve_self_lexical(name);
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::SetLocal(slot));
        } else if name.starts_with('!') && name.len() > 1 {
            let slot = self.alloc_local(name);
            self.code.emit(OpCode::SetLocal(slot));
        } else {
            let idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::SetGlobal(idx));
        }
    }

    /// Emit the declaration-time type-constraint registration op for a
    /// `my TYPE $x`-family declaration. A `my`/`state` declaration lexically
    /// inside a routine, or directly inside a plain `{ ... }` block (see
    /// `lexically_in_block`), gets `SetVarTypeScoped` — env-only
    /// registration, exactly like a typed parameter — so its constraint dies
    /// with the frame/block instead of leaking onto a same-named variable
    /// elsewhere through the global name-keyed store
    /// (`todo/deep/bare-name-type-constraint-store-is-scope-blind.md`).
    ///
    /// ADR-0042 slice 1: `@`/`%` containers now use the same scoped opcode as
    /// scalars. Their element/key metadata is embedded directly on the
    /// container VALUE (`ArrayData`/`HashData`) by the mutation chokepoints
    /// (`element_constraint_for` / `container_type_metadata`), not read
    /// through the global map at the hot push/subscript paths any more — the
    /// global-map registration this doc comment used to require for those
    /// paths is no longer their source of truth, so containers no longer need
    /// to be excluded from scoping. `&` (routines are not scoped this way)
    /// stays excluded. Everything else keeps the both-store `SetVarType`:
    /// `our` (package-scoped, outlives the frame), dynamics (`$*x`, read
    /// cross-frame by design), an anonymous scalar (`my T $`, name
    /// `__ANON_STATE__` — stored via `SetGlobal`/`GetGlobal` rather than a
    /// local slot, immediately consumed into a `WrapTypedContainer` cell, so
    /// there is no per-declaration slot for a block/frame exit to scope;
    /// going through the env-only opcode here starved that read of the
    /// global-map registration it depends on — see
    /// `t/pair-typed-value-container.t`), and mainline declarations outside
    /// any block (no frame/scope to scope to).
    fn emit_set_var_type(&mut self, name: &str, name_idx: u32, tc_idx: u32, is_our: bool) {
        let scoped = !is_our
            && (self.is_routine || self.lexically_in_routine || self.lexically_in_block)
            && !name.starts_with('&')
            && !name.starts_with('*')
            && name != "__ANON_STATE__"
            && !name.contains("::");
        if scoped {
            self.code
                .emit(OpCode::SetVarTypeScoped { name_idx, tc_idx });
        } else {
            self.code.emit(OpCode::SetVarType { name_idx, tc_idx });
        }
    }

    /// For a genuine assignment (`$*x = ...`) to a dynamic variable, emit a
    /// runtime guard that throws X::Dynamic::NotFound when the dynamic var is
    /// not in scope (Raku requires `my $*x` first). Called ONLY from the plain
    /// `Stmt::Assign` / `Expr::AssignExpr` paths — never from parameter binding,
    /// element auto-vivification, or `my` declarations — so those legitimately
    /// introduce a fresh dynamic var without tripping the guard. No-op for any
    /// non-dynamic name (the sigil-stripped form must begin with the `*` twigil).
    fn maybe_emit_dynamic_var_check(&mut self, name: &str) {
        let bare = name.trim_start_matches(['$', '@', '%', '&']);
        if let Some(rest) = bare.strip_prefix('*')
            && !rest.is_empty()
            && !self.local_map.contains_key(name)
        {
            // Use the same (possibly package-qualified) key the store uses so the
            // runtime env lookup in CheckDynamicVarDeclared matches the declared
            // var's env entry exactly.
            let key = self.qualify_variable_name(name);
            let idx = self.code.add_constant(Value::str(key));
            self.code.emit(OpCode::CheckDynamicVarDeclared(idx));
        }
    }

    /// Push the current value of a named scalar variable, mirroring the slot
    /// resolution of `emit_set_named_var` (local slot if present, else global).
    fn emit_get_named_var(&mut self, name: &str) {
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::GetLocal(slot));
        } else {
            let idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::GetGlobal(idx));
        }
    }

    /// Compile arguments for a `**@`-slurpy output routine (`say`/`put`/`print`/
    /// `note`). Such a routine keeps each argument whole: a `.Slip`/`slip(...)`
    /// VALUE prints as a list (`say @a.Slip` → `(1 2 3)`), it does NOT flatten
    /// into separate arguments. Only an explicit `|EXPR` pipe flattens at the
    /// call site (`say |@a` → `123`). So every non-pipe argument gets a `DeSlip`
    /// that demotes a top-level Slip value to a Seq, dodging the op's
    /// Slip-flatten pass; `|EXPR` args (already a `MakeSlip`) are left to flatten.
    fn compile_slurpy_out_args(&mut self, exprs: &[Expr]) {
        for expr in exprs {
            // These statement-form listops have the same `**@` / `**%`
            // split as their routine forms.  Preserve the in-band named
            // marker for a colonpair (or bareword fat-arrow) written directly
            // in their argument list, so the I/O op can leave it out of the
            // positional output.  A grouped Pair is deliberately not this
            // shape and remains printable data (ADR-0021).
            if matches!(expr, Expr::Binary { op, .. } if *op == TokenKind::FatArrow) {
                self.mint_named_pair = true;
            }
            self.compile_expr(expr);
            if !Self::is_slip_interpolation_arg(expr) {
                self.code.emit(OpCode::DeSlip);
            }
        }
    }

    /// Whether `expr` is a `|EXPR` argument-list interpolation marker.
    ///
    /// ADR-0054: this is the ONLY thing that makes an argument spread into
    /// the caller's argument list. A `Slip` VALUE an ordinary argument
    /// merely evaluates to (`f(@a.Slip)`) is one argument, not a spread
    /// request — the compiler is the only place that can tell the two
    /// apart, since by the time the VM sees the value the `|` is gone.
    pub(super) fn is_slip_interpolation_arg(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Unary {
                op: TokenKind::Pipe,
                ..
            }
        )
    }

    fn positional_arg_source_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::CodeVar(name) => Some(format!("&{}", name)),
            Expr::BareWord(name) => Some(name.to_string()),
            // DoStmt wrapping a VarDecl: `my $c = 42` passed as argument
            Expr::DoStmt(stmt) => Self::extract_varname_from_stmt(stmt),
            // For FatArrow (named args like `:into(%h)`), encode "key=varname"
            // so the VM can write back to the variable after a builtin call.
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } => {
                let key = match left.as_ref() {
                    Expr::Literal(lit) => lit.as_str(),
                    Expr::BareWord(s) => Some(s.as_str()),
                    _ => None,
                };
                let val_name = Self::extract_inner_varname(right);
                if let (Some(k), Some(v)) = (key, val_name) {
                    Some(format!("{}={}", k, v))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract variable name from an expression, including through DoStmt/SyntheticBlock.
    fn extract_inner_varname(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::CodeVar(name) => Some(format!("&{}", name)),
            Expr::DoStmt(stmt) => Self::extract_varname_from_stmt(stmt),
            _ => None,
        }
    }

    /// Extract variable name from a statement, handling VarDecl and SyntheticBlock.
    fn extract_varname_from_stmt(stmt: &Stmt) -> Option<String> {
        match stmt {
            Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
            Stmt::SyntheticBlock(stmts) => {
                for s in stmts {
                    if let Some(name) = Self::extract_varname_from_stmt(s) {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn add_arg_sources_constant(&mut self, args: &[Expr]) -> Option<u32> {
        let mut entries = Vec::with_capacity(args.len());
        for arg in args {
            if Self::is_slip_interpolation_arg(arg) {
                // ADR-0054 S1/S2 (third entry shape): a `|EXPR` position
                // spreads into zero or more runtime arguments, so it carries
                // no single traceable rw source -- mark it with a sentinel
                // distinct from "no source" (`NIL`), `Str(name)` and
                // `Pair(name, Int(slot))`. `decode_arg_sources` returns these
                // positions so a call op can spread by call-site syntax
                // instead of the argument's runtime Slip-shape.
                entries.push(Value::TRUE);
                continue;
            }
            if let Expr::DoStmt(stmt) = arg
                && let Stmt::VarDecl {
                    name,
                    is_our: false,
                    ..
                } = stmt.as_ref()
            {
                let shadows_outer = self.enclosing_local_names.contains(name)
                    || self.local_scopes.len() >= 2
                        && self.local_scopes[..self.local_scopes.len() - 1]
                            .iter()
                            .any(|scope| scope.contains_key(name));
                if shadows_outer {
                    self.declare_local(name);
                }
            }
            entries.push(if let Some(name) = Self::positional_arg_source_name(arg) {
                // §1.4/§1.5: bake the caller's local slot for a plain source var
                // as `Pair(name, Int(slot))`, so the rw-arg writeback can target
                // the LIVE (inner shadow) slot instead of the by-name `position`
                // (outer) slot. A source with no local slot, or an encoded
                // `key=var` named form, stays a bare `Str(name)`. Decoders extract
                // the name from either shape, so existing consumers are unchanged.
                match self.local_map.get(&name) {
                    Some(&slot) if !name.contains('=') => {
                        Value::pair(name, Value::int(slot as i64))
                    }
                    _ => Value::str(name),
                }
            } else {
                Value::NIL
            });
        }
        if entries.iter().all(|v| v.is_nil()) {
            None
        } else {
            Some(self.code.add_constant(Value::array(entries)))
        }
    }

    /// Bake the `|EXPR` positions of a `Stmt::Call`-shaped (`CallArg`) argument
    /// list into the constant pool, for `ExecCallPairs`.
    ///
    /// ADR-0054 Slice 4: this uses the SAME per-position entry shape
    /// `add_arg_sources_constant` uses for an `Expr`-list call site (`TRUE`
    /// for a `|EXPR` position, `NIL` otherwise — decoded by
    /// `decode_arg_slip_positions`), rather than the separate "array of bare
    /// integer positions" encoding the retired `add_slip_positions_constant`
    /// used. `ExecCallPairs` has no rw-arg source tracking (it never did, so
    /// this does not add `Str`/`Pair` name entries the way
    /// `add_arg_sources_constant` does for `CallFunc`/`CallMethod`/etc.) — a
    /// call site now carries exactly one syntax descriptor instead of two
    /// parallel constants. `None` (no `|` argument) is the common case.
    fn add_call_arg_sources_constant(&mut self, args: &[CallArg]) -> Option<u32> {
        let mut entries = Vec::with_capacity(args.len());
        let mut has_slip = false;
        for arg in args {
            if matches!(arg, CallArg::Slip(_)) {
                entries.push(Value::TRUE);
                has_slip = true;
            } else {
                entries.push(Value::NIL);
            }
        }
        if has_slip {
            Some(self.code.add_constant(Value::array(entries)))
        } else {
            None
        }
    }

    /// Whether this loop parameter is a *named* slurpy (`*%h`). It absorbs named
    /// arguments only, so it neither consumes a positional element of the
    /// iteration chunk nor makes the block's `.count` `Inf`.
    fn for_param_is_named_slurpy(name: &str, def: &crate::ast::ParamDef) -> bool {
        def.is_variadic() && name.strip_prefix('\\').unwrap_or(name).starts_with('%')
    }

    /// The number of source elements one iteration of a `for` loop consumes.
    ///
    /// Rakudo keys this on the block's `.count`: `for`/`map` batch `count`
    /// elements per call, except that a `count` of `Inf` — which any *positional*
    /// slurpy (`*@r`, `**@r`, `+@r`, `|c`) produces — or a `count` below 2 falls
    /// back to one element at a time. Optional and defaulted non-slurpy params do
    /// count (`-> $a, $b = 9` batches in twos, the short final chunk triggering
    /// the default), and a named slurpy is invisible to the positional count
    /// (`-> $a, $b, *%h` still batches in twos). So `-> $a, $b, *@rest` batches
    /// ONE element and then dies with "Too few positionals passed", exactly as
    /// rakudo does.
    fn for_chunk_arity(params: &[String], params_def: &[crate::ast::ParamDef]) -> u32 {
        let mut positionals = 0u32;
        for (i, name) in params.iter().enumerate() {
            match params_def.get(i) {
                Some(def) if Self::for_param_is_named_slurpy(name, def) => continue,
                Some(def) if def.is_variadic() => return 1,
                _ => positionals += 1,
            }
        }
        positionals.max(1)
    }

    /// Whether a `for` header's *explicit* signature declares zero positional
    /// parameters, i.e. rakudo's `.count` for that block is 0.
    ///
    /// The loop still hands such a block one element per iteration, so rakudo
    /// dies on the very first invocation -- before the body runs once -- with
    /// "Too many positionals passed; expected 0 arguments but got 1". An empty
    /// source invokes the block zero times and is therefore fine.
    ///
    /// Two spellings reach it. The parser flags `-> { ... }` directly
    /// (`explicit_zero_params`, also set for the statement-modifier spelling's
    /// `-> { ... } for LIST` and `sub () { ... } for LIST`). A signature whose
    /// only parameters are *named* slurpies (`-> *%h { ... }`) binds named
    /// arguments only and likewise never takes a positional; `for_chunk_arity`
    /// already skips those for the same reason.
    ///
    /// A block with NO signature at all (`for LIST { ... }`, `{ ... } for LIST`)
    /// is not zero-count: it binds the topic and has `.count` 1.
    fn for_zero_positional_params(
        explicit_zero_params: bool,
        param: &Option<String>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
    ) -> bool {
        if explicit_zero_params {
            return true;
        }
        if param.is_some() || params.is_empty() {
            return false;
        }
        params.iter().enumerate().all(|(i, name)| {
            params_def
                .get(i)
                .is_some_and(|def| Self::for_param_is_named_slurpy(name, def))
        })
    }

    fn build_for_bind_stmts(
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        param_idx: Option<u32>,
        params: &[String],
        params_def: &[crate::ast::ParamDef],
        rw_block: bool,
    ) -> Vec<Stmt> {
        let bind_stmt = |name: String, expr: Expr| {
            // A destructured signature parameter DECLARES its target, so a
            // dynamic-twigil target (`:value($*PATH)`) must be introduced as a
            // fresh dynamic var (VarDecl with is_dynamic), not treated as a bare
            // assignment to a pre-existing dynamic var (which would wrongly throw
            // X::Dynamic::NotFound). `&` targets likewise declare.
            let is_dynamic_target = name
                .trim_start_matches(['$', '@', '%', '&'])
                .starts_with('*');
            if name.starts_with('&') || is_dynamic_target {
                Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: is_dynamic_target,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                }
            } else {
                Stmt::Assign {
                    name,
                    expr,
                    op: AssignOp::Assign,
                }
            }
        };

        // A destructured sub-signature target (`-> % [:@dists]`, `-> @ ($a,@b)`)
        // is a fresh block-scoped lexical that must SHADOW any outer variable of
        // the same name -- a plain `Stmt::Assign` would instead resolve up the
        // scope chain and clobber the outer var (e.g. zef's
        // `my Candidate @dists = gather for @x -> % [:@dists] {...}`, where the
        // inner `:@dists` collided with the outer typed `@dists`). Declare it.
        let decl_stmt = |name: String, expr: Expr| {
            let is_dynamic_target = name
                .trim_start_matches(['$', '@', '%', '&'])
                .starts_with('*');
            Stmt::VarDecl {
                name,
                expr,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: is_dynamic_target,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            }
        };

        let mut bind_stmts = Vec::new();
        if let Some(single_param) = param
            && param_idx.is_none()
        {
            bind_stmts.push(bind_stmt(single_param.clone(), Expr::Var("_".to_string())));
        }
        // Unpack a destructuring pattern out of the value already bound to
        // `target_name`. Used both for the single-pattern pointy block
        // (`-> [$a, $b]`, whose target is the whole iteration value) and for each
        // pattern of a multi-parameter one (`-> [$a, $b], [$c, $d]`, whose
        // targets are the per-element synthetic params bound further below).
        let destructure_binds = |target_name: String,
                                 sub_params: &[crate::ast::ParamDef],
                                 bind_stmts: &mut Vec<Stmt>| {
            let mut positional_index = 0usize;
            for sub in sub_params {
                if sub.name.is_empty() {
                    continue;
                }
                if sub.named {
                    // Named destructuring `:$key` binds via the accessor method
                    // when the object provides one (Pair.key/.value, object
                    // attribute readers), otherwise by hash key (Hash/Map, which
                    // have no method named after an arbitrary key). Decide at
                    // runtime: `$_.^can("key") ?? $_.key !! $_<key>`.
                    //
                    // A scalar named sub-param `:$curi` stores its name sigil-
                    // stripped ("curi"), but an `@`/`%` named sub-param `:@dists`
                    // keeps its sigil in `sub.name` ("@dists"). The accessor and
                    // hash key must use the *key* name ("dists"), so strip a
                    // leading array/hash sigil (and any twigil) before looking up;
                    // the sigil is kept only for the bind target below so the
                    // value lands in an `@`/`%` container.
                    let after_sigil = sub
                        .name
                        .strip_prefix('@')
                        .or_else(|| sub.name.strip_prefix('%'))
                        .unwrap_or(&sub.name);
                    let lookup_name = after_sigil
                        .strip_prefix('!')
                        .or_else(|| after_sigil.strip_prefix('.'))
                        .unwrap_or(after_sigil)
                        .to_string();
                    let method_call = Expr::MethodCall {
                        target: Box::new(Expr::Var(target_name.clone())),
                        name: Symbol::intern(&lookup_name),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                    };
                    let hash_lookup = Expr::Index {
                        target: Box::new(Expr::Var(target_name.clone())),
                        index: Box::new(Expr::Literal(Value::str(lookup_name.clone()))),
                        is_positional: false,
                    };
                    let method_result = Expr::Ternary {
                        cond: Box::new(Expr::MethodCall {
                            target: Box::new(Expr::Var(target_name.clone())),
                            name: Symbol::intern("can"),
                            args: vec![Expr::Literal(Value::str(lookup_name.clone()))],
                            modifier: Some('^'),
                            quoted: false,
                        }),
                        then_expr: Box::new(method_call),
                        else_expr: Box::new(hash_lookup),
                    };
                    // If the named param has a sub_signature (e.g. :key($k)),
                    // bind to the sub_signature variable instead of the param name.
                    if let Some(inner_params) = &sub.sub_signature {
                        for inner in inner_params {
                            if !inner.name.is_empty() {
                                bind_stmts
                                    .push(decl_stmt(inner.name.clone(), method_result.clone()));
                            }
                        }
                    } else {
                        // An `@`-sigil named sub-param binds like a signature
                        // parameter: it flattens the (Positional) value's elements
                        // into the array (shallow), unlike plain `my @x = $val`
                        // assignment which keeps an itemized List as one element.
                        // e.g. zef's `-> % [:@dists]` over `dists => $repo.installed`
                        // (a 1-element List) must yield `@dists[0]` = the dist, not
                        // a List wrapping it. `.list` gives the shallow flatten.
                        let target_expr = if sub.name.starts_with('@') {
                            Expr::MethodCall {
                                target: Box::new(method_result),
                                name: Symbol::intern("list"),
                                args: Vec::new(),
                                modifier: None,
                                quoted: false,
                            }
                        } else {
                            method_result
                        };
                        bind_stmts.push(decl_stmt(sub.name.clone(), target_expr));
                    }
                } else if sub.slurpy && sub.sigilless {
                    // |rest capture parameter: collect remaining elements into a Capture
                    // Generates: rest = \(|target[positional_index..*])
                    let slice_expr = Expr::Index {
                        target: Box::new(Expr::Var(target_name.clone())),
                        index: Box::new(Expr::Binary {
                            left: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                            op: crate::token_kind::TokenKind::DotDot,
                            right: Box::new(Expr::Whatever),
                        }),
                        is_positional: true,
                    };
                    let capture_expr = Expr::CaptureLiteral(vec![Expr::Unary {
                        op: crate::token_kind::TokenKind::Pipe,
                        expr: Box::new(slice_expr),
                    }]);
                    // Positional destructure targets keep `Stmt::Assign` binding:
                    // a fresh `my` declaration would copy an `is raw` / `is default`
                    // container and drop its `.VAR.default` (roast
                    // S02-names/is_default.t `-> (..., %a is raw, ...)`). Only the
                    // NAMED branch above declares (to shadow an outer same-named
                    // var, which positional destructure does not need).
                    bind_stmts.push(bind_stmt(sub.name.clone(), capture_expr));
                    // No need to increment positional_index; capture consumes all remaining
                } else {
                    let element_expr = Expr::Index {
                        target: Box::new(Expr::Var(target_name.clone())),
                        index: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                        is_positional: false,
                    };
                    // An optional destructure param (`-> ($a, $b?)`) seeds its
                    // type object (Mu for untyped — this is a block) when the
                    // source has no element at this slot; a default binds the
                    // default expression instead.
                    let value_expr = if sub.default.is_some() || sub.optional_marker {
                        let fallback = match &sub.default {
                            Some(default_expr) => default_expr.clone(),
                            None => {
                                let mut marked = sub.clone();
                                marked.mark_block_param();
                                Expr::Literal(
                                    crate::runtime::Interpreter::missing_optional_param_value(
                                        &marked,
                                    ),
                                )
                            }
                        };
                        Expr::Ternary {
                            cond: Box::new(Expr::Binary {
                                left: Box::new(Expr::MethodCall {
                                    target: Box::new(Expr::Var(target_name.clone())),
                                    name: Symbol::intern("elems"),
                                    args: Vec::new(),
                                    modifier: None,
                                    quoted: false,
                                }),
                                op: crate::token_kind::TokenKind::Gt,
                                right: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                            }),
                            then_expr: Box::new(element_expr),
                            else_expr: Box::new(fallback),
                        }
                    } else {
                        element_expr
                    };
                    bind_stmts.push(bind_stmt(sub.name.clone(), value_expr));
                    positional_index += 1;
                }
            }
        };
        if let Some(def) = param_def
            && let Some(sub_params) = &def.sub_signature
        {
            destructure_binds(
                param.as_deref().unwrap_or("_").to_string(),
                sub_params,
                &mut bind_stmts,
            );
        }
        // `_.elems` on the per-iteration chunk array — the number of source
        // elements that actually flowed into this batch.
        let chunk_elems = || Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern("elems"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
        };
        // Multi-param pointy blocks (`-> $a, $b = 7`) carry a full ParamDef per
        // param. A param is *required* when it has neither an optional marker
        // (`$x?`) nor a default (`$x = expr`). When the final chunk is shorter
        // than the required count, Raku throws "Too few positionals passed"
        // mid-loop (after the full chunks have run). Emit that guard so the
        // body sees it before any bind, matching Raku's batching semantics.
        if !params_def.is_empty() {
            // A slurpy binds whatever is left over, so it is never *required* and
            // never contributes to the "expected N arguments" count -- but a
            // positional slurpy does turn the bound into a lower one ("at least
            // N"), matching rakudo's wording.
            let positional: Vec<&crate::ast::ParamDef> = params_def
                .iter()
                .enumerate()
                .filter(|(i, d)| {
                    !Self::for_param_is_named_slurpy(
                        params.get(*i).map(String::as_str).unwrap_or(""),
                        d,
                    )
                })
                .map(|(_, d)| d)
                .collect();
            let has_positional_slurpy = positional.iter().any(|d| d.is_variadic());
            let required_arity = positional
                .iter()
                .filter(|d| d.default.is_none() && !d.optional_marker && !d.is_variadic())
                .count();
            let total = positional.iter().filter(|d| !d.is_variadic()).count();
            if required_arity > 0 {
                // Rakudo words an open-ended bound as "expected at least N
                // arguments but got only M".
                let expected = if has_positional_slurpy {
                    format!(
                        "expected at least {} arguments but got only ",
                        required_arity
                    )
                } else if required_arity == total {
                    format!("expected {} arguments but got ", total)
                } else {
                    format!(
                        "expected {} or {} arguments but got ",
                        required_arity, total
                    )
                };
                let msg = Expr::Binary {
                    left: Box::new(Expr::Literal(Value::str(format!(
                        "Too few positionals passed; {}",
                        expected
                    )))),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(chunk_elems()),
                };
                bind_stmts.push(Stmt::If {
                    cond: Expr::Binary {
                        left: Box::new(chunk_elems()),
                        op: crate::token_kind::TokenKind::Lt,
                        right: Box::new(Expr::Literal(Value::int(required_arity as i64))),
                    },
                    then_branch: vec![Stmt::Die(msg)],
                    else_branch: Vec::new(),
                    binding_var: None,
                    is_statement_modifier: false,
                });
            }
        }
        // When `$_` is one of the multi-param names (e.g. `-> $_, $name`),
        // binding it first would clobber the source array before other params
        // can read from it.  Defer the `$_` binding to the end.
        let mut deferred_topic = None;
        // Which chunk element the next *positional* param binds. A named slurpy
        // (`*%h`) consumes none, so it must not shift the params after it.
        let mut positional_slot = 0usize;
        for (i, p) in params.iter().enumerate() {
            // Sigilless params are prefixed with \\ by the parser.
            let actual_name = p.strip_prefix('\\').unwrap_or(p).to_string();
            // A slurpy binds a *list*, not one chunk element: `*%h`/`+%h` gets the
            // named arguments (always none — a `for` loop passes only
            // positionals), and every other variadic (`*@r`, `**@r`, `+@r`, and
            // the sigilless capture `|c`) gets whatever is left of the chunk.
            let slurpy_kind = params_def
                .get(i)
                .filter(|d| d.is_variadic())
                .map(|d| !Self::for_param_is_named_slurpy(p, d));
            let slurpy_expr = slurpy_kind.map(|is_positional_slurpy| {
                if is_positional_slurpy {
                    // `_.skip(n).Array` — a fresh per-iteration Array holding the
                    // unconsumed tail of the chunk (empty when nothing is left).
                    Expr::MethodCall {
                        target: Box::new(Expr::MethodCall {
                            target: Box::new(Expr::Var("_".to_string())),
                            name: Symbol::intern("skip"),
                            args: vec![Expr::Literal(Value::int(positional_slot as i64))],
                            modifier: None,
                            quoted: false,
                        }),
                        name: Symbol::intern("Array"),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                    }
                } else {
                    Expr::Hash(Vec::new())
                }
            });
            let slot = positional_slot;
            if slurpy_kind.is_none() {
                positional_slot += 1;
            }
            // A param with a default value (`-> $a, $b = 7`) binds to the source
            // element when the chunk has one at this slot, else to the default.
            // Use an explicit `_.elems > slot` test (not `// default`) so a
            // present but undefined element is still bound, matching Raku.
            let element_expr = Expr::Index {
                target: Box::new(Expr::Var("_".to_string())),
                index: Box::new(Expr::Literal(Value::int(slot as i64))),
                is_positional: false,
            };
            // An optional param without a default (`-> $a, $b? {}`) seeds its
            // type object (Mu for untyped block params) when the chunk is
            // short, like an unpassed optional in a routine call.
            let missing_expr = match params_def.get(i) {
                Some(d) if d.default.is_some() => Some(d.default.clone().unwrap()),
                Some(d) if d.optional_marker => Some(Expr::Literal(
                    crate::runtime::Interpreter::missing_optional_param_value(d),
                )),
                _ => None,
            };
            let has_fallback = missing_expr.is_some();
            let value_expr = match missing_expr {
                Some(fallback) => Expr::Ternary {
                    cond: Box::new(Expr::Binary {
                        left: Box::new(chunk_elems()),
                        op: crate::token_kind::TokenKind::Gt,
                        right: Box::new(Expr::Literal(Value::int(slot as i64))),
                    }),
                    then_expr: Box::new(element_expr),
                    else_expr: Box::new(fallback),
                },
                None => element_expr,
            };
            // A slurpy ignores the per-element/default machinery above entirely.
            let value_expr = slurpy_expr.unwrap_or(value_expr);
            // An `@`-sigil multi-param de-itemizes the chunk element: Raku binds
            // `@a` to the element's *list* (`for $@n, Any -> @a, $T` → `@a` IS the
            // 4-element array), whereas a plain assignment would wrap an itemized
            // array as a one-element array (`[$@n]`, elems=1). `DeitemizeForBind`
            // de-itemizes like `.list` but preserves the source array's element
            // type, so a typed chunk element (`array[int]`) keeps its type instead
            // of collapsing to an untyped `Array`. (A scalar param keeps plain
            // assignment + later MarkReadonly; `%`-sigil is left as plain
            // assignment — `.hash` mis-coerces an itemized hash.)
            let value_expr = if actual_name.starts_with('@') && slurpy_kind.is_none() {
                Expr::DeitemizeForBind(Box::new(value_expr))
            } else {
                value_expr
            };
            // An `@`/`%`-sigil multi-param loop variable is its own fresh
            // per-iteration lexical in Raku, not an alias of a same-named
            // outer `@`/`%` (unlike a plain `Stmt::Assign`, which mutates
            // whatever container the shared slot already holds in place —
            // exactly the outer container when the names collide). Declare
            // it instead, so each iteration gets a genuinely fresh container
            // and the outer variable is shadowed rather than clobbered. See
            // todo/tickets/for-multi-param-array-hash-shadow-clobbers-outer-container.md.
            // `MarkBind` (the same marker `my @a := expr` uses) makes the
            // declaration a raw bind rather than a `my @a = expr`-style
            // coercing assignment, which would collapse an already
            // element-typed source array (`array[int8]`) to a plain `Array`.
            // ADR-0045 row 16: a WRITABLE scalar multi-parameter must bind the
            // chunk element RAW, for the same reason the `@`/`%` case does.
            // `for @a.kv -> $i, $v is rw` has to alias the source element, and a
            // plain `Stmt::Assign` reads the chunk slot through the ordinary
            // element chokepoint, which decontainerizes -- so the cell the
            // producer handed out arrived at `$v` as a bare value and the write
            // was lost, while the writeback that used to carry it had already
            // been retired for the iteration precisely BECAUSE the chunk carried
            // a cell. `MarkBind` + a declaration is the shape that does not
            // coerce, and `array_slot_ref` is idempotent, so binding
            // `_[1]` over a chunk holding a source cell aliases the SOURCE
            // element rather than the temporary chunk.
            //
            // Restricted to a plain positional with no default: a slurpy binds a
            // fresh list and a defaulted parameter may bind the default instead
            // of an element, neither of which is an alias of anything.
            let rw_scalar_alias = slurpy_kind.is_none()
                && !has_fallback
                && !actual_name.starts_with(['@', '%', '&'])
                && (rw_block
                    || params_def
                        .get(i)
                        .is_some_and(|d| d.sigilless || d.traits.iter().any(|t| t == "rw")));
            let stmt = if actual_name.starts_with(['@', '%']) || rw_scalar_alias {
                Stmt::SyntheticBlock(vec![
                    Stmt::MarkBind,
                    decl_stmt(actual_name.clone(), value_expr),
                ])
            } else {
                bind_stmt(actual_name.clone(), value_expr)
            };
            if actual_name == "_" {
                deferred_topic = Some(stmt);
            } else {
                bind_stmts.push(stmt);
            }
        }
        if let Some(stmt) = deferred_topic {
            bind_stmts.push(stmt);
        }
        // A multi-parameter pointy block may destructure any of its parameters
        // (`-> [$target, $variant], [$expected, $desc]`). The parameter itself is
        // bound from its chunk element above; unpack it now that it holds a value.
        for (i, def) in params_def.iter().enumerate() {
            if let Some(sub_params) = &def.sub_signature
                && let Some(name) = params.get(i)
            {
                destructure_binds(
                    name.strip_prefix('\\').unwrap_or(name).to_string(),
                    sub_params,
                    &mut bind_stmts,
                );
            }
        }
        // Sigilless multi-params (`-> \k, \v`) are raw bindings that alias the
        // source element directly; in Raku they are writable and modifications
        // propagate back to the source (`for @a -> \k, \v { v = ... }` mutates
        // @a, `for %h.kv -> \k, \v { v = ... }` writes back through the value).
        // The caller treats any sigilless for-param as rw (see `has_sigilless`
        // in `compile_stmt`), so they must NOT be marked readonly here.
        bind_stmts
    }

    /// Whether every item a `for` over `iterable` yields is provably a bare
    /// value with no container of its own — see
    /// [`crate::opcode::ForLoopSpec::source_items_are_bare`]. Conservative: it
    /// answers `true` only for shapes that can never produce a container.
    pub(crate) fn for_iterable_yields_bare_items(iterable: &Expr) -> bool {
        match iterable {
            Expr::Grouped(inner) => Self::for_iterable_yields_bare_items(inner),
            // A single literal (`for 5 { $_ = 1 }`) aliases the literal itself.
            // A container-valued literal is excluded: its elements are the
            // items, and those are containers (`for [1,2]` parses as
            // `BracketArray`, but a folded constant could reach here).
            Expr::Literal(v) => !matches!(
                v.view(),
                crate::value::ValueView::Array(..) | crate::value::ValueView::Hash(..)
            ),
            // A list built entirely out of literals (`for 1, 2`, `for <a b>`).
            Expr::ArrayLiteral(items) => {
                !items.is_empty() && items.iter().all(|i| matches!(i, Expr::Literal(_)))
            }
            // A `Range` yields immutable endpoints-derived values and never
            // element containers, whatever its endpoints are -- `for $a..$b
            // -> $v is rw` fails to bind in raku exactly as `for 1..2` does.
            Expr::Binary { op, .. } => matches!(
                op,
                crate::token_kind::TokenKind::DotDot
                    | crate::token_kind::TokenKind::DotDotCaret
                    | crate::token_kind::TokenKind::CaretDotDot
                    | crate::token_kind::TokenKind::CaretDotDotCaret
            ),
            // `.keys` on a container yields freshly built keys, never the
            // container's element cells. Restricted to `@`/`%` variables so a
            // user-defined `keys` method returning containers is not affected.
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty() && *name == "keys" => {
                matches!(target.as_ref(), Expr::ArrayVar(_) | Expr::HashVar(_))
            }
            _ => false,
        }
    }

    fn for_iterable_source_name(iterable: &Expr) -> Option<String> {
        match iterable {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayLiteral(items) if items.len() == 1 => match &items[0] {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                _ => None,
            },
            // Handle @a.values, @a.kv, @a.pairs, $pair.value → source is @a / $pair
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty()
                && (*name == "values" || *name == "kv" || *name == "value" || *name == "pairs") =>
            {
                Self::for_iterable_source_name(target)
            }
            // Handle @a.reverse → source is @a (reversed)
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty() && *name == "reverse" => Self::for_iterable_source_name(target),
            // `@$h` desugars to `($h).list`: the loop iterates the scalar's
            // inner array and must alias its elements (`$_ .= uc for @$hdr`
            // uppercases in place — Text::CSV's header munge). Tag the source
            // SIGILED ("$h") so the runtime's per-element writeback recognizes
            // the deref'd-container shape, while the whole-topic scalar
            // writeback (keyed on the bare name, `for $x {...}`) stays off.
            // `@a.list` re-tags the array itself, same as bare `for @a`.
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty() && *name == "list" => {
                let inner = match target.as_ref() {
                    Expr::Grouped(g) => g.as_ref(),
                    other => other,
                };
                match inner {
                    Expr::Var(name) => Some(crate::env::sigiled_scalar_name(name)),
                    Expr::ArrayVar(name) => Some(format!("@{}", name)),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Check if the for-loop iterable involves a `.reverse` call on a container.
    fn for_iterable_is_reversed(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::MethodCall { name, args, .. }
                if args.is_empty() && *name == "reverse"
        )
    }

    /// Extract per-element variable names when the iterable is a list of
    /// scalar variables (e.g. `($a, $b, $c)`). Returns an empty vec otherwise.
    /// The bare array variable name for a `for @a` loop (iterable is a single
    /// plain `@`-variable), used for live-array iteration. `None` for anything
    /// else (a transform, a literal list, a scalar, an index expression, …).
    fn for_single_array_source(iterable: &Expr) -> Option<String> {
        match iterable {
            Expr::ArrayVar(name) => Some(name.clone()),
            _ => None,
        }
    }

    fn for_direct_smartmatch(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::Binary {
                op: crate::token_kind::TokenKind::SmartMatch,
                ..
            }
        )
    }

    /// Bake the local slot for a `for @a` live-array source (§1.5). The source
    /// is an `@`-variable by construction (`for_single_array_source` only
    /// matches `Expr::ArrayVar`), so the `@`-sigiled local key is tried FIRST:
    /// a bare-name-first lookup resolved `for @in` to a same-named scalar
    /// param `$in` (scalar locals are stored sigil-less), making the VM's
    /// live-array growth check read the wrong container and re-run the last
    /// iteration (Text::CSV 90_csv.t emitted the final row twice).
    /// `None` when the source is not a local (keeps the VM's env fallback).
    fn for_single_array_source_local(&self, source: &Option<String>) -> Option<u32> {
        let name = source.as_ref()?;
        self.local_map
            .get(&format!("@{name}"))
            .or_else(|| self.local_map.get(name))
            .copied()
    }

    /// Bake the local slot for each per-element writeback target of a
    /// `for ($a, $b, $c) { ... }` loop (§1.5). `None` for a name that has no
    /// local slot at this point (`our`/global/undeclared), which keeps the
    /// runtime by-name writeback path. Parallel to the `source_var_names` list.
    fn for_source_var_locals(&self, names: &[String]) -> Vec<Option<u32>> {
        names
            .iter()
            .map(|name| self.local_map.get(name).copied())
            .collect()
    }

    fn for_iterable_var_names(iterable: &Expr) -> Vec<String> {
        // A single parenthesized scalar (`for ($x) -> $v is rw`) reaches here as
        // `Grouped(Var)`; unwrap it so the per-element rw writeback targets `$x`.
        if let Expr::Grouped(inner) = iterable
            && let Expr::Var(name) = inner.as_ref()
        {
            return vec![name.clone()];
        }
        if let Expr::ArrayLiteral(items) = iterable {
            let names: Vec<String> = items
                .iter()
                .filter_map(|item| match item {
                    Expr::Var(name) => Some(name.clone()),
                    _ => None,
                })
                .collect();
            if names.len() == items.len() {
                return names;
            }
        }
        Vec::new()
    }

    /// Detect if the iterable is a `.kv` method call (key-value pairs).
    fn for_iterable_is_kv(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::MethodCall { name, args, .. }
                if args.is_empty() && *name == "kv"
        )
    }

    /// Detect if the for-loop iterable's outermost transform yields `Pair`
    /// objects that *wrap* the source element (`.pairs`/`.antipairs`), so the
    /// loop variable is a `Pair`, not the element. The per-element source
    /// writeback must be disabled for these (it would overwrite the element
    /// with the Pair); the Pair's rw `.value` alias propagates instead.
    /// `.kv`/`.values` are excluded (`.values` IS the element; `.kv` has its
    /// own `kv_mode` writeback).
    fn for_iterable_wraps_pair(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::MethodCall { name, args, .. }
                if args.is_empty() && (*name == "pairs" || *name == "antipairs")
        )
    }

    /// Detect `%h.values` / `$b.values` on a variable: the loop variable aliases
    /// the container's *value*, so a plain (`$_` / named) topic writeback must
    /// update the source by key order. `$_ = X for %h.values` mutates `%h`;
    /// `$_ = X for $b.values` mutates a mutable QuantHash (MixHash/BagHash). The
    /// VM branches on the runtime container type (Hash vs Mix/Bag/Set). Bare
    /// `for %h` iterates Pairs (no value writeback) and `.keys` yields read-only
    /// keys, so this is intentionally narrow: only the `.values` transform.
    /// (`@a.values` already writes back via the `@`-source path; the flag is a
    /// harmless no-op there.)
    fn for_iterable_is_values_alias(iterable: &Expr) -> bool {
        if let Expr::MethodCall {
            target, name, args, ..
        } = iterable
            && args.is_empty()
            && *name == "values"
        {
            return Self::for_iterable_source_name(target).is_some();
        }
        false
    }

    /// Whether `$x` is a scalar *item container* — the thing that makes
    /// `for $x` iterate once and `@a = $x` produce a one-element array.
    /// `constant $x` and a `:=`-bind to a non-itemized value install no Scalar
    /// container, and neither do the `$=...` Pod document variables: rakudo's
    /// `$=pod` is an `Array` bound to the collected document, so `for $=pod`
    /// iterates the `Pod::*` blocks rather than yielding the whole document
    /// once (`todo/tickets/dollar-equals-pod-item-not-iterable-block-object`).
    pub(super) fn scalar_var_is_item_container(&self, name: &str) -> bool {
        !name.starts_with('=')
            && !self.constant_vars.contains(name)
            && !self.noncontainer_bound_vars.contains(name)
    }

    fn normalize_for_iterable(&self, iterable: &Expr) -> Expr {
        match iterable {
            // Scalar variables are item containers in `for` and should not be flattened.
            // Exception: `constant $x`, a `:=`-bound-to-non-itemized `$x` and the
            // `$=...` Pod document variables bind without a Scalar container, so
            // `for $x` iterates the elements (like sigilless variables).
            Expr::Var(name) if self.scalar_var_is_item_container(name) => {
                Expr::ArrayLiteral(vec![iterable.clone()])
            }
            // A parenthesized single scalar (`for ($x)`) reaches here as
            // `Grouped(Var)`; iterate it once, exactly like the bare `for $x`.
            Expr::Grouped(inner)
                if matches!(inner.as_ref(), Expr::Var(name)
                    if self.scalar_var_is_item_container(name)) =>
            {
                Expr::ArrayLiteral(vec![(**inner).clone()])
            }
            _ => iterable.clone(),
        }
    }

    /// Compile a compilation unit.
    ///
    /// Constant folding (ADR-0006 §2.1) is only valid while no user-defined
    /// operator is declared in the unit — a `sub infix:<+>` overrides even
    /// native `Int + Int`. Declarations can hide anywhere (a nested block, a sub
    /// body, a class body), so instead of statically walking the AST for them,
    /// the declaration sites flag the shared `FoldCtx` as they are compiled and
    /// the unit is recompiled here, folding disabled, if one turned up after
    /// something had already been folded. Only files that declare operators pay
    /// the second pass.
    pub(crate) fn compile(self, stmts: &[Stmt]) -> (CompiledCode, CompiledFns) {
        if !self.fold_root || !self.fold_ctx.is_enabled() {
            return self.compile_unit(stmts);
        }
        let pristine = self.clone();
        let ctx = std::sync::Arc::clone(&self.fold_ctx);
        let compiled = self.compile_unit(stmts);
        if !ctx.needs_refold_pass() {
            return compiled;
        }
        let mut retry = pristine;
        retry.fold_ctx = std::sync::Arc::new(const_fold::FoldCtx::disabled());
        retry.compile_unit(stmts)
    }

    fn compile_unit(mut self, stmts: &[Stmt]) -> (CompiledCode, CompiledFns) {
        // Hoist top-level `use Test` declarations to the front (Raku `use` is
        // BEGIN-time, so test functions are available throughout the file even
        // when `plan`/`ok` appear textually before `use Test;`).
        let test_hoisted;
        let stmts = if let Some(r) = Self::hoist_test_use_decls(stmts) {
            test_hoisted = r;
            &test_hoisted[..]
        } else {
            stmts
        };
        // Reorder stub class declarations so real definitions come right
        // after stubs (Raku class declarations are compile-time).
        let reordered;
        let stmts = if let Some(r) = Self::reorder_stub_class_decls(stmts) {
            reordered = r;
            &reordered[..]
        } else {
            stmts
        };
        // A placeholder variable ($^x, @_, ...) directly in the mainline is
        // outside any sub or block -> X::Placeholder::Mainline. Emit the Die
        // first so it fires before any other statement runs.
        if self.is_mainline
            && let Some(ph) = crate::ast::collect_unattached_placeholders(stmts)
                .into_iter()
                .next()
        {
            let err = crate::method_signature_shared::placeholder_scope_error("mainline", &ph);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            self.code.compute_needs_env_sync();
            return (self.code, self.compiled_functions);
        }
        // A `unit module Foo;` puts the whole rest of the compilation unit in
        // package Foo. Switch the *runtime* package before the hoist pass, not
        // when the declaration's own opcodes run: the hoisted `RegisterSub`s are
        // emitted first, and registration is keyed off the runtime package, so
        // hoisting under GLOBAL would install a second, bare-named copy of every
        // routine that stays callable from the consumer's scope (PLAN 8.22).
        // The compiler's own `current_package` is still switched by the
        // declaration itself, so this must not qualify anything here.
        if let Some(name_idx) = self.unit_package_name_const(stmts) {
            self.code.emit(OpCode::SetCurrentPackage { name_idx });
        }
        self.hoist_sub_decls(stmts, false);
        // Pre-register declaration-only shells of class/role declarations so a
        // mainline statement that runs before the textual declaration can
        // already construct the type (Raku type declarations are compile-time;
        // see `hoist_type_decl_shells`).
        self.hoist_type_decl_shells(stmts);
        // Register `our` subs declared inside nested blocks early so they are
        // reachable via `OUR::` before their declaring block runs (Raku
        // installs `our sub`s into the package at compile time).
        self.hoist_nested_our_subs(stmts);
        // Hoist `my TYPE $var;` type constraints (see `hoist_typed_var_decls`).
        self.hoist_typed_var_decls(stmts);
        // If the top-level body contains a CATCH or CONTROL block, wrap in
        // an implicit try so the phaser can observe exceptions / control
        // signals from the surrounding statements.
        let has_catch = stmts
            .iter()
            .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_)));
        if has_catch {
            self.compile_implicit_try(stmts);
            self.code.emit(OpCode::SetTopic);
        } else if self.is_routine && Self::has_block_enter_leave_phasers(stmts) {
            self.compile_phaser_block_scope(stmts, PhaserBlockResult::ReturnViaTopic);
        } else {
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                if is_last {
                    match stmt {
                        Stmt::Expr(expr) => {
                            // Tail expression becomes the body value -> escapes.
                            // (`compile_routine_tail_expr`: an `is rw` routine
                            // body recompiled through the interpreter carrier
                            // still returns its tail's container, ADR-0059.)
                            self.with_escape(true, |c| c.compile_routine_tail_expr(expr));
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::Call { name, args } => {
                            // Tail call: its value is the body result, whether
                            // the args are positional-only or carry named/slip
                            // args (compile_tail_stmt_call_value handles both).
                            self.compile_tail_stmt_call_value(*name, args);
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                            // ADR-0048 D3/D6: a tail bare `{ ... }` statement is
                            // still a Block invoked with zero arguments, so it
                            // gets raku's arity failure -- retiring the ad-hoc
                            // "Implicit placeholder parameters are not available
                            // in bare nested blocks" string that used to live
                            // here. `SyntheticBlock` is a parser desugar wrapper,
                            // not a source block, so it is excluded: its
                            // placeholders belong to the enclosing routine.
                            if matches!(stmt, Stmt::Block(_))
                                && self.emit_inlined_body_placeholder_binds(body, ArgSupply::None)
                            {
                                continue;
                            }
                            // A tail block carrying ENTER/LEAVE/KEEP/UNDO/PRE/POST
                            // phasers must run them: inlining (below) drops the
                            // phasers entirely (e.g. a trailing `{ ...; LEAVE
                            // unlink $f }` never cleaned up). Route it through a
                            // real `BlockScope` instead, which also delivers the
                            // block value via the topic (matching the SetTopic the
                            // inline path emits).
                            if Self::has_block_enter_leave_phasers(body) {
                                self.compile_phaser_block_scope(
                                    body,
                                    PhaserBlockResult::ReturnViaTopic,
                                );
                                continue;
                            }
                            // A genuine source `{ ... }` is a Raku callframe; a
                            // compiler-synthesized block is not.
                            if matches!(stmt, Stmt::Block(_)) {
                                self.compile_bare_block_inline(body);
                            } else {
                                // `stmt` is a `SyntheticBlock` here (e.g. the
                                // parser's wrapper for a tail `my $*x := ...`
                                // bind) -- a compiler wrapper, not a real
                                // lexical scope. Use the transparent helper so
                                // an earlier dynamic-var read anywhere in this
                                // mainline is still visible to the wrapped
                                // declaration's own X::Dynamic::Postdeclaration
                                // check.
                                self.compile_synthetic_block_inline(body);
                            }
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::If {
                            cond,
                            then_branch,
                            else_branch,
                            binding_var,
                            is_statement_modifier,
                        } => {
                            self.compile_if_value(
                                cond,
                                then_branch,
                                else_branch,
                                binding_var,
                                *is_statement_modifier,
                            );
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::VarDecl { name, .. } => {
                            // VarDecl as last statement: compile normally, then
                            // load the declared variable back and set as topic
                            // so that implicit return works correctly.
                            let var_name = name.clone();
                            self.compile_stmt(stmt);
                            let slot = self.alloc_local(&var_name);
                            self.code.emit(OpCode::GetLocal(slot));
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        _ => {}
                    }
                }
                self.compile_stmt(stmt);
                // `given`/`when`/`default` leave their block value on the stack
                // even in statement (sink) position — the tail-statement arms
                // above rely on that leaked value being the block result. A
                // *non-last* one must have it popped, or it would shadow the
                // block's real tail value (the stack top wins over the topic).
                if !is_last
                    && matches!(
                        stmt,
                        Stmt::Given { .. } | Stmt::When { .. } | Stmt::Default(_)
                    )
                {
                    self.code.emit(OpCode::Pop);
                }
            }
        }
        self.code.compute_needs_env_sync();
        (self.code, self.compiled_functions)
    }

    /// Compile a lexical block scope containing ENTER/LEAVE/KEEP/UNDO/PRE/POST
    /// phasers (`OpCode::BlockScope`). Shared by the top-level routine body,
    /// `Stmt::Block` statements, and `do`-block expressions so the phaser/value
    /// semantics (trailing ENTER as block value, `SetLine`-marker handling) stay
    /// in one place.
    ///
    /// `mode` selects how the block's trailing value is disposed of — see
    /// [`PhaserBlockResult`]. Do NOT use `ReturnViaTopic` for a same-frame
    /// statement context (a bare block statement, an `if`/`given` body): real
    /// Raku never sets `$_` from a block's own trailing statement value (`{
    /// 1; 2 }` does not make `$_` become `2`), and `SetTopic` there would
    /// clobber whatever `$_` the enclosing scope (e.g. a `given`'s
    /// topicalized value) already has live, since such a body shares the
    /// current frame's topic register rather than getting a fresh one. It is
    /// only safe for a routine's own compiled body, which always runs in its
    /// own fresh call frame (see `news/2026-08/given-if-block-scope-topic-clobber.md`).
    pub(super) fn compile_phaser_block_scope(&mut self, stmts: &[Stmt], mode: PhaserBlockResult) {
        let idx = self.code.emit(OpCode::BlockScope {
            pre_end: 0,
            enter_end: 0,
            body_end: 0,
            keep_start: 0,
            undo_start: 0,
            post_start: 0,
            end: 0,
            is_bare_block: false,
        });
        Self::compile_pre_phasers(self, stmts);
        self.code.patch_block_pre_end(idx);
        // When the textually-last statement of the block is an ENTER phaser, its
        // entry-time value becomes the block's result value (Raku semantics).
        // Capture that value in the ENTER section via PushEnterResult and load it
        // back as the block result at the end of the body via LoadEnterResult.
        // Ignore trailing `SetLine` markers when locating the last statement.
        let last_idx = stmts
            .iter()
            .rposition(|s| !matches!(s, Stmt::SetLine(_)))
            .unwrap_or(usize::MAX);
        let last_is_enter = matches!(
            stmts.get(last_idx),
            Some(Stmt::Phaser {
                kind: PhaserKind::Enter,
                ..
            })
        );
        for (i, s) in stmts.iter().enumerate() {
            if let Stmt::Phaser {
                kind: PhaserKind::Enter,
                body,
                ..
            } = s
            {
                // ADR-0048 Phase 2: `ENTER {}` does not take a signature in
                // raku. This function is the only place an ENTER body is
                // compiled (extracted from the enclosing block's statement
                // list before `compile_stmt` ever sees the wrapping
                // `Stmt::Phaser`), so the check lives here.
                if self.emit_block_placeholder_die(body) {
                    continue;
                }
                if last_is_enter && i == last_idx {
                    // Compile so the body's final statement leaves its value on the
                    // stack, then move it onto the ENTER-result stack.
                    if body.is_empty() {
                        self.compile_expr(&Expr::Literal(Value::NIL));
                    } else {
                        for (j, inner) in body.iter().enumerate() {
                            if j == body.len() - 1 {
                                match inner {
                                    Stmt::Expr(expr) => self.compile_expr(expr),
                                    _ => {
                                        self.compile_stmt(inner);
                                        self.compile_expr(&Expr::Literal(Value::TRUE));
                                    }
                                }
                            } else {
                                self.compile_stmt(inner);
                            }
                        }
                    }
                    self.code.emit(OpCode::PushEnterResult);
                } else {
                    for inner in body {
                        self.compile_stmt(inner);
                    }
                }
            }
        }
        self.code.patch_block_enter_end(idx);
        let body_stmts: Vec<&Stmt> = stmts
            .iter()
            .filter(|s| {
                !matches!(
                    s,
                    Stmt::Phaser {
                        kind: PhaserKind::Enter
                            | PhaserKind::Leave
                            | PhaserKind::Keep
                            | PhaserKind::Undo
                            | PhaserKind::Pre
                            | PhaserKind::Post,
                        ..
                    }
                )
            })
            .collect();
        if last_is_enter {
            // The block result comes from the trailing ENTER phaser, so none of the
            // (earlier) non-phaser body statements provide the value; compile them
            // all in sink context and materialize the captured ENTER value.
            for s in body_stmts.iter() {
                self.compile_stmt(s);
            }
            self.code.emit(OpCode::LoadEnterResult);
            // `Discard` behaves like `Push` here (the value stays on the VM
            // stack through the LEAVE/KEEP/UNDO/POST sections below, so their
            // truthy/falsy KEEP-vs-UNDO check and any POST-phaser read of it
            // still see the real value) -- the trailing `Pop` that actually
            // discards it is emitted once, after those sections, at the very
            // end of this function.
            if matches!(mode, PhaserBlockResult::ReturnViaTopic) {
                self.code.emit(OpCode::SetTopic);
            }
        } else {
            // The value-producing statement is the last *non-marker* statement:
            // trailing `SetLine` markers (emitted between statements once real line
            // numbers differ) must not become the block's value, or a phaser-only
            // block would yield a spurious `True` and run KEEP instead of UNDO.
            let last_value_idx = body_stmts
                .iter()
                .rposition(|s| !matches!(s, Stmt::SetLine(_)));
            for (i, s) in body_stmts.iter().enumerate() {
                if Some(i) == last_value_idx {
                    match mode {
                        // `Discard` leaves the value on the stack too (see the
                        // trailing `Pop` at the end of this function) so the
                        // LEAVE/KEEP/UNDO/POST sections below can still see it.
                        PhaserBlockResult::Push | PhaserBlockResult::Discard => {
                            self.compile_last_stmt_as_value(s)
                        }
                        // A routine body's tail (ADR-0059: an `is rw` routine
                        // with LEAVE/ENTER phasers still returns its tail's
                        // container); every other tail shape is shared with
                        // the `let`/`do` block helper.
                        PhaserBlockResult::ReturnViaTopic => {
                            if let Stmt::Expr(expr) = s {
                                self.with_escape(true, |c| c.compile_routine_tail_expr(expr));
                                self.code.emit(OpCode::SetTopic);
                            } else {
                                self.compile_last_stmt_as_topic(s)
                            }
                        }
                    }
                } else {
                    self.compile_stmt(s);
                }
            }
            // A phaser-only block (no value-producing statement) still needs a
            // value on the stack: for `Push`, the consumer needs one; for
            // `Discard`, the trailing `Pop` at the end of this function always
            // runs and needs something to pop.
            if matches!(mode, PhaserBlockResult::Push | PhaserBlockResult::Discard)
                && last_value_idx.is_none()
            {
                self.emit_nil_value();
            }
        }
        self.code.patch_block_body_end(idx);
        self.code.patch_block_keep_start(idx);
        {
            let mut prev_guard: Option<usize> = None;
            for s in stmts.iter().rev() {
                if let Stmt::Phaser { kind, body, .. } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                {
                    if let Some(pg) = prev_guard {
                        self.code.patch_leave_guard_next(pg);
                    }
                    let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                    // ADR-0048 Phase 2: `LEAVE {}`/`KEEP {}` do not take a
                    // signature in raku.
                    if !self.emit_block_placeholder_die(body) {
                        for inner in body {
                            self.compile_stmt(inner);
                        }
                    }
                    prev_guard = Some(guard_idx);
                }
            }
            if let Some(pg) = prev_guard {
                self.code.patch_leave_guard_next(pg);
            }
        }
        self.code.patch_block_undo_start(idx);
        {
            let mut prev_guard: Option<usize> = None;
            for s in stmts.iter().rev() {
                if let Stmt::Phaser { kind, body, .. } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                {
                    if let Some(pg) = prev_guard {
                        self.code.patch_leave_guard_next(pg);
                    }
                    let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                    // ADR-0048 Phase 2: `LEAVE {}`/`UNDO {}` do not take a
                    // signature in raku.
                    if !self.emit_block_placeholder_die(body) {
                        for inner in body {
                            self.compile_stmt(inner);
                        }
                    }
                    prev_guard = Some(guard_idx);
                }
            }
            if let Some(pg) = prev_guard {
                self.code.patch_leave_guard_next(pg);
            }
        }
        self.code.patch_block_post_start(idx);
        Self::compile_post_phasers(self, stmts);
        self.code.patch_loop_end(idx);
        // `Discard` kept the value on the stack through the sections above (so
        // KEEP/UNDO's truthy check and POST's topic read see the real value);
        // now that they have run, actually discard it.
        if matches!(mode, PhaserBlockResult::Discard) {
            self.code.emit(OpCode::Pop);
        }
    }
}
