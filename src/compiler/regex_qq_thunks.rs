//! Compile-time lowering of a regex literal's interpolating double-quoted
//! atoms (`/"x @a[0]"/`) to qq thunks — see [`crate::regex_qq_atoms`].

use super::*;
use crate::runtime::meta_ns::MetaNs;
use crate::value::ValueView;

impl Compiler {
    /// Load a non-`Nil`/`Bool` literal. A code-bearing regex literal is a
    /// closure over the scope it is written in, so it loads through
    /// `LoadRegexClosure` instead of a plain constant — see that op's doc
    /// comment. Kept out of line: `compile_expr` recurses deeply, and every
    /// local here would otherwise widen each of its frames.
    #[inline(never)]
    pub(super) fn compile_literal_constant(&mut self, v: &Value) {
        let topic = self.regex_literal_topic_capture(v);
        self.compile_literal_constant_with_topic(v, topic);
    }

    #[inline(never)]
    pub(super) fn compile_literal_constant_with_topic(&mut self, v: &Value, topic: Option<u32>) {
        let idx = self.code.add_constant(v.clone());
        let mut captures = self.regex_literal_closure_captures(v);
        let qq_thunks = self.compile_regex_qq_thunks(v);
        if !qq_thunks.is_empty() {
            captures.get_or_insert_with(Vec::new).extend(qq_thunks);
        }
        if captures.is_some() || topic.is_some() {
            self.code.emit(OpCode::LoadRegexClosure {
                const_idx: idx,
                topic,
                captures: std::sync::Arc::new(captures.unwrap_or_default()),
            });
        } else {
            self.code.emit(OpCode::LoadConst(idx));
        }
    }

    /// Compile one closure per interpolating `"..."` atom of the regex
    /// literal `v`, each into a fresh local, and return the capture entries
    /// (`MetaNs::RegexQq` key, local slot) that put them on the regex value's
    /// scope. Empty when the literal has no such atom.
    ///
    /// The body goes through the one qq-string interpolation parser
    /// (`parse_dispatch::parse_qq_interpolation`) and is compiled as an
    /// ordinary block, so its variables, subscripts, method calls and
    /// embedded blocks resolve in the literal's defining scope exactly as the
    /// same `"..."` would outside a regex.
    pub(super) fn compile_regex_qq_thunks(&mut self, v: &Value) -> Vec<(Symbol, u32)> {
        self.compile_qq_thunk_bodies(Self::regex_qq_thunk_bodies(v))
    }

    /// [`Compiler::compile_regex_qq_thunks`] for a pattern that is not a
    /// regex value: the raw source text of an `s///` / `S///` pattern.
    /// `None` when it has no interpolating `"..."` atom.
    pub(super) fn compile_pattern_qq_thunks(
        &mut self,
        pattern: &str,
    ) -> Option<std::sync::Arc<Vec<(Symbol, u32)>>> {
        let captures = self.compile_qq_thunk_bodies(crate::regex_qq_atoms::thunk_bodies(pattern));
        (!captures.is_empty()).then(|| std::sync::Arc::new(captures))
    }

    fn compile_qq_thunk_bodies(&mut self, bodies: Vec<String>) -> Vec<(Symbol, u32)> {
        let mut captures = Vec::new();
        for body in bodies {
            let Some(thunk) = Self::regex_qq_thunk_expr(&body) else {
                continue;
            };
            let name = format!("__mutsu_regex_qq_{}", self.code.locals.len());
            let slot = self.alloc_fresh_local(&name);
            self.compile_expr(&thunk);
            self.code.emit(OpCode::SetLocal(slot));
            captures.push((MetaNs::RegexQq.key(Symbol::intern(&body)), slot));
        }
        captures
    }

    /// The closure a `"..."` atom's `body` lowers to: the body read as a qq
    /// string, as a block. `None` when the body interpolates nothing.
    pub(super) fn regex_qq_thunk_expr(body: &str) -> Option<Expr> {
        let expr = crate::parse_dispatch::parse_qq_interpolation(body);
        if matches!(expr, Expr::Literal(_)) {
            return None;
        }
        Some(Expr::AnonSub {
            body: vec![Stmt::Expr(expr)],
            is_rw: false,
            is_raw: false,
            is_block: true,
        })
    }

    /// The thunks of a `token`/`rule` declaration's body (the raw
    /// `Stmt::Expr(Expr::Literal(regex))` payload, ADR-0009), compiled into
    /// this frame's locals like a regex literal's: the capture entries to
    /// add to the declaration plan's `regex_captures`. Empty for a
    /// declaration with parameters — they are bound at match time, where a
    /// thunk built here cannot see them.
    pub(super) fn compile_token_decl_qq_thunks(
        &mut self,
        params: &[String],
        body: &[Stmt],
    ) -> Vec<(Symbol, u32)> {
        if !params.is_empty() {
            return Vec::new();
        }
        let bodies = Self::token_decl_qq_thunk_bodies(body);
        self.compile_qq_thunk_bodies(bodies)
    }

    /// [`Compiler::compile_token_decl_qq_thunks`] for a declaration with no
    /// frame to compile into (a class or role body): one standalone chunk per
    /// thunk, which registration runs in the declaring scope (see
    /// [`crate::opcode::CompiledTokenDeclPlan::qq_thunk_chunks`]).
    pub(super) fn token_decl_qq_thunk_chunks(
        &self,
        params: &[String],
        body: &[Stmt],
    ) -> Vec<(Symbol, crate::opcode::CompiledDeclExpr)> {
        if !params.is_empty() {
            return Vec::new();
        }
        Self::token_decl_qq_thunk_bodies(body)
            .into_iter()
            .filter_map(|body| {
                let thunk = Self::regex_qq_thunk_expr(&body)?;
                let key = MetaNs::RegexQq.key(Symbol::intern(&body));
                Some((key, self.compile_decl_expr(&thunk)))
            })
            .collect()
    }

    /// The `"..."` atom bodies of a `token`/`rule` declaration's raw body.
    pub(super) fn token_decl_qq_thunk_bodies(body: &[Stmt]) -> Vec<String> {
        body.iter()
            .find_map(|stmt| match stmt {
                Stmt::Expr(Expr::Literal(v)) => Some(Self::regex_qq_thunk_bodies(v)),
                _ => None,
            })
            .unwrap_or_default()
    }

    /// The `"..."` atom bodies of regex literal `v` that
    /// [`Compiler::compile_regex_qq_thunks`] lowers.
    pub(super) fn regex_qq_thunk_bodies(v: &Value) -> Vec<String> {
        let pattern = match v.view() {
            ValueView::Regex(p) => p.as_str().to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.as_str().to_string(),
            _ => return Vec::new(),
        };
        // An anonymous `token ($x) { ... }` binds its parameters at match
        // time, where a thunk built here cannot see them.
        if v.regex_signature().is_some() {
            return Vec::new();
        }
        crate::regex_qq_atoms::thunk_bodies(&pattern)
    }
}
