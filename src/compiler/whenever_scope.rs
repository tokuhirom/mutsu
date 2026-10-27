//! Compiling a `whenever` statement (`OpCode::WheneverScope`).
//!
//! The body and each of its `LAST` / `QUIT` phasers compile here, at the
//! definition site, as ordinary closures (#12872). Their free variables are
//! then captured the way any closure's are — vouched captures, shared cells
//! for the ones a sibling writes — instead of being re-resolved by name when a
//! carrier recompiles the body's AST on every dispatch, which read a closure's
//! captured `$x` from whichever invocation of the same body called it.

use super::*;
use crate::symbol::Symbol;

impl Compiler {
    pub(super) fn compile_whenever(
        &mut self,
        supply: &Expr,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) {
        self.compile_expr(supply);
        // A whenever block without a pointy param may still declare its
        // parameter as a placeholder (`whenever $ch { %^content.kv }`): the
        // emitted value binds to it, arity-1 like `-> $v`.
        let params: Vec<String> = if params.is_empty() && param_defs.is_empty() {
            crate::ast::collect_placeholders_shallow(body)
                .into_iter()
                .take(1)
                .collect()
        } else {
            params.to_vec()
        };
        // `LAST` and `QUIT` are callbacks of the subscription, not statements
        // of the body: they run once, when the source completes or fails.
        let mut main = Vec::new();
        let mut phaser_bodies: Vec<(bool, Vec<Stmt>)> = Vec::new();
        for stmt in body {
            match stmt {
                Stmt::Phaser {
                    kind: kind @ (PhaserKind::Last | PhaserKind::Quit),
                    body: phaser_body,
                    ..
                } => phaser_bodies.push((*kind == PhaserKind::Quit, phaser_body.clone())),
                _ => main.push(stmt.clone()),
            }
        }
        let (body_idx, body_cc_idx) = self.add_whenever_closure(&params, param_defs, main);
        let phasers: Box<[crate::opcode::WheneverPhaser]> = phaser_bodies
            .into_iter()
            .map(|(is_quit, phaser_body)| {
                let (body_idx, cc_idx) = self.add_whenever_closure(&[], &[], phaser_body);
                crate::opcode::WheneverPhaser {
                    is_quit,
                    body_idx,
                    cc_idx,
                }
            })
            .collect();
        self.code.emit(OpCode::WheneverScope {
            body_idx,
            body_cc_idx,
            phasers,
            yields_value: self.do_stmt_yields_value,
        });
    }

    /// Compile one `whenever` callback as a pointy-block closure and stash its
    /// signature and body in the stmt pool, as `MakeLambda` does. Returns
    /// `(stmt_pool index, closure_compiled_codes index)`.
    fn add_whenever_closure(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: Vec<Stmt>,
    ) -> (u32, u32) {
        let mut compiled = self.compile_closure_body(params, param_defs, &body);
        compiled.is_pointy_block = true;
        // The callback outlives the statement: it is dispatched whenever the
        // source emits, from whichever frame or thread does so.
        let cc_idx = self.add_closure_code_baked(compiled, true);
        let body_idx = self.code.add_stmt(Stmt::SubDecl {
            is_hidden_from_backtrace: false,
            spans: Default::default(),
            name: Symbol::intern(""),
            name_expr: None,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: None,
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body,
            multi: false,
            is_rw: false,
            is_raw: false,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            scope: crate::ast::SubScope::My,
            custom_traits: Vec::new(),
        });
        (body_idx, cc_idx)
    }
}
