use super::*;
use crate::ast::{CallArg, make_anon_sub};
use crate::symbol::Symbol;

impl Compiler {
    /// Compile the single operand of `return-rw` so that it yields a *container*
    /// rather than a decontainerized value.
    ///
    /// An `is rw` routine's contract is that it hands its caller something
    /// writable; `sub g(\c) is rw { return-rw c<a> }; g(%h) = 1` must write the
    /// element of the caller's `%h`. A subscript is therefore compiled in the
    /// same container-producing mode a `:=` bind RHS uses
    /// (`scalar_bind_autovivify` + `bind_terminal`), which promotes the element
    /// to its shared `ContainerRef` cell (or hands back a deferred
    /// `HashEntryRef` for a not-yet-existent hash key, so the eventual write
    /// autovivifies the path). `assign_lvalue_container` at the call site writes
    /// through whichever of those comes back.
    ///
    /// A plain scalar lexical operand (ADR-0059 Slice 2) is compiled to the
    /// variable's *shared cell* (`WrapVarRef` + `CaptureVarCell`, the same
    /// capture a List literal element gets), so `sub f() { return-rw $v }` hands
    /// the caller `$v`'s container. Ordinary reads of the result decontainerize
    /// at the usual chokepoints (`GetLocal`'s `into_deref`, element reads), so
    /// `my $x = f()` still observes a value; only a binding (`my $r := f()`) or
    /// an element write through a returned list keeps the container.
    ///
    /// An inline declaration operand (`return-rw my $x = 1`) is the same shape
    /// once the declaration has run: the value is on the stack and the local
    /// slot exists, so the identical two-op tail boxes that slot. The cell
    /// outlives the callee frame because it is a GC'd `Gc<crate::value::ContainerCell>`, not a
    /// frame reference.
    pub(super) fn compile_return_rw_arg(&mut self, arg: &Expr) {
        let saved_rw = self.rw_return_operand;
        self.rw_return_operand = true;
        match arg {
            Expr::Index { .. } | Expr::MultiDimIndex { .. } => {
                let saved_av = self.scalar_bind_autovivify;
                let saved_terminal = self.bind_terminal;
                let saved_raw_list_elem = self.raw_list_elem_terminal;
                self.scalar_bind_autovivify = true;
                self.bind_terminal = true;
                // The caller writes through whatever comes back, so an
                // immutable `List`'s scalar element must arrive raw: rakudo
                // refuses `sub g(\c) is rw { return-rw c[0] }; g((1, 2)) = 9`
                // with "Cannot modify an immutable Int (1)", where promoting the
                // element to a private cell made the write silently succeed and
                // reach nobody (`Crane::In`'s `Positional:D` descent, and hence
                // `Crane.set` on an immutable `List`).
                self.raw_list_elem_terminal = true;
                self.compile_expr(arg);
                self.scalar_bind_autovivify = saved_av;
                self.bind_terminal = saved_terminal;
                self.raw_list_elem_terminal = saved_raw_list_elem;
            }
            // `$flag ?? c<x> !! c<y>`: the condition is an ordinary value read;
            // each arm is itself a location the routine may hand back, so both
            // compile in container mode (raku: an `is rw` routine whose tail is
            // a ternary over two elements assigns through the taken branch).
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.rw_return_operand = saved_rw;
                self.compile_expr(cond);
                self.rw_return_operand = true;
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                self.compile_return_rw_arg(then_expr);
                let jump_end = self.code.emit(OpCode::Jump(0));
                self.code.patch_jump(jump_else);
                self.compile_return_rw_arg(else_expr);
                self.code.patch_jump(jump_end);
            }
            // `method acc is rw { $!v }`: the tail names the *attribute's*
            // storage, and a method frame reads it out of a seeded local slot
            // whose cell would be disconnected from the instance. Emit the
            // op that promotes `self`'s own attribute slot instead — the same
            // promotion a public accessor read gets in `:=` context, so the
            // two spellings name one container (see `OpCode::AttrContainerRef`).
            Expr::Var(name) if Self::rw_tail_attribute_name(name).is_some() => {
                self.compile_expr(arg);
                let attr = Self::rw_tail_attribute_name(name).expect("checked above");
                let idx = self.code.add_constant(Value::str(attr));
                self.code.emit(OpCode::AttrContainerRef(idx));
            }
            // `sub f() is rw { $obj.acc }`: a public `is rw` auto-accessor
            // names the attribute's Scalar, so the rw routine hands that
            // container back (raku: `f() = 4` writes `$obj.acc`). The request
            // is the same `MarkAccessorRefContext` a `:=` bind RHS emits; its
            // consumer (`try_fast_accessor_read`'s `want_ref` branch, or the
            // wrapped-accessor terminal) answers with the promoted attribute
            // cell only for a zero-argument read of a public `is rw` scalar
            // accessor and hands every other callee's value back unchanged.
            Expr::MethodCall {
                args,
                modifier: None,
                ..
            } if args.is_empty() && self.return_rw_container_name(arg).is_none() => {
                self.compile_expr(arg);
                self.mark_trailing_method_call_as_accessor_ref();
            }
            _ => {
                let cell_name = self.return_rw_container_name(arg);
                // A readonly `$` parameter operand has no container to hand
                // out (#11108); flag its value for the call assignment.
                let readonly_param = Self::scalar_container_alias_name(arg)
                    .is_some_and(|n| self.readonly_scalar_params.contains(n));
                self.compile_expr(arg);
                if let Some(name) = cell_name {
                    self.emit_wrap_var_ref(&name);
                    self.code.emit(OpCode::CaptureVarCell);
                }
                if readonly_param {
                    self.code.emit(OpCode::MarkReadonlyRwTail);
                }
            }
        }
        self.rw_return_operand = saved_rw;
    }

    /// The bare attribute name a `$!attr` rw-tail exposes (`v` for `$!v`).
    ///
    /// The parser spells `$!v` as `Expr::Var("!v")`. Deliberately narrow: only
    /// the `$` sigil (an `@!a` / `%!h` tail is `ArrayVar` / `HashVar`, whose
    /// value is already a shared container reached by its own accessor path),
    /// and only a plain identifier after the twigil, so nothing else that
    /// happens to start with `!` is boxed.
    fn rw_tail_attribute_name(name: &str) -> Option<String> {
        let rest = name.strip_prefix('!')?;
        let first = rest.chars().next()?;
        let plain = (first.is_ascii_alphabetic() || first == '_')
            && rest
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-');
        plain.then(|| rest.to_string())
    }

    /// The lexical name a `return-rw` operand denotes the *container* of, when
    /// that container is a plain scalar variable's own cell: a bare `$v` (or
    /// `$v.item`, which Raku defines as handing the invocant's container back),
    /// and an inline `my $x = ...` declaration, whose slot is live by the time
    /// the operand's value reaches the stack.
    ///
    /// A **sigilless** lexical (`sub f(\x) is raw { x }`, `my \y := ...`) names
    /// the same kind of storage location a `$`-sigiled one does, but the parser
    /// gives it `Expr::BareWord` rather than `Expr::Var` — a bareword is also
    /// how a type name, an enum value and a listop-less call are spelled, so it
    /// is only a container reference when it actually names a sigilless binding
    /// visible to the frame being compiled. The compiler's local and enclosing
    /// sigilless sets record that distinction, so consult them rather than
    /// guessing from the spelling.
    ///
    /// Deliberately NOT folded into `scalar_container_alias_name`: that is an
    /// associated function, and only the sigilless-binding probes here can tell
    /// a sigilless *variable* from the type name / enum value / listop-less call
    /// a bareword otherwise spells.
    pub(super) fn sigilless_local_container_name(&self, arg: &Expr) -> Option<String> {
        let Expr::BareWord(name) = arg else {
            return None;
        };
        (Self::is_plain_lexical_name(name)
            && (self.sigilless_locals.contains(name) || self.enclosing_sigilless.contains(name)))
        .then(|| name.clone())
    }

    /// Whether a bareword names an in-scope lexical term -- a sigilless
    /// binding of this or an enclosing frame, or a `constant` -- rather than a
    /// type, package or not-yet-declared listop.
    pub(super) fn bareword_is_lexical_term(&self, name: &str) -> bool {
        self.sigilless_locals.contains(name)
            || self.enclosing_sigilless.contains(name)
            || self.constant_vars_in_scope.contains(name)
            || self.constant_value(name).is_some()
    }

    /// Deliberately narrow. `@`/`%`/`&`-sigiled names, twigils, attributes and
    /// package-qualified names are excluded: their containers are reached by
    /// their own machinery, and boxing them into a scalar cell here would leak a
    /// `ContainerRef` past the consumers that only decontainerize at the scalar
    /// chokepoints.
    fn return_rw_container_name(&self, arg: &Expr) -> Option<String> {
        if let Some(name) = Self::scalar_container_alias_name(arg)
            && Self::is_plain_lexical_name(name)
        {
            return Some(name.to_string());
        }
        if let Some(name) = self.sigilless_local_container_name(arg) {
            return Some(name);
        }
        if let Expr::DoStmt(stmt) = arg
            && let Stmt::VarDecl { name, .. } = stmt.as_ref()
            && Self::is_plain_lexical_name(name)
        {
            return Some(name.clone());
        }
        None
    }

    /// Compile a subscript call argument as the element's container, for the
    /// lvalue chain of a `return-rw` operand (see `rw_return_operand`). This is
    /// the single-dimension twin of the `MultiDimIndexBindRef` argument path
    /// above: the element is promoted to its shared `ContainerRef` cell (a
    /// missing hash key yields the deferred `HashEntryRef` token instead, so a
    /// read is still non-vivifying), and a `\raw` / `is rw` parameter binds that
    /// container rather than a value snapshot. Without it the recursive descent
    /// of a path-addressing routine writes into a detached copy.
    fn compile_rw_chain_index_arg(&mut self, arg: &Expr) {
        let saved_av = self.scalar_bind_autovivify;
        let saved_terminal = self.bind_terminal;
        self.scalar_bind_autovivify = true;
        self.bind_terminal = true;
        self.compile_expr(arg);
        self.scalar_bind_autovivify = saved_av;
        self.bind_terminal = saved_terminal;
    }

    pub(super) fn is_normalized_stmt_call_name(name: &str) -> bool {
        matches!(
            name,
            "shift"
                | "pop"
                | "push"
                | "unshift"
                | "append"
                | "prepend"
                | "splice"
                | "undefine"
                | "VAR"
                | "indir"
        ) || super::compile_inputs::is_imported_function(name)
    }

    pub(super) fn rewrite_stmt_call_args(name: &str, args: &[CallArg]) -> Vec<CallArg> {
        let rewrites_needed = matches!(
            name,
            "lives-ok"
                | "dies-ok"
                | "exits-ok"
                | "throws-like"
                | "warns-like"
                | "doesn't-warn"
                | "is_run"
        );
        if !rewrites_needed {
            return args.to_vec();
        }
        let mut positional_index = 0usize;
        args.iter()
            .map(|arg| match arg {
                CallArg::Positional(expr) => {
                    let rewritten = if matches!(
                        name,
                        "lives-ok"
                            | "dies-ok"
                            | "exits-ok"
                            | "throws-like"
                            | "warns-like"
                            | "doesn't-warn"
                    ) && positional_index == 0
                    {
                        match expr {
                            Expr::Block(body) => make_anon_sub(body.clone()),
                            _ => expr.clone(),
                        }
                    } else if name == "is_run" && positional_index == 1 {
                        Self::rewrite_hash_block_values(expr)
                    } else {
                        expr.clone()
                    };
                    positional_index += 1;
                    CallArg::Positional(rewritten)
                }
                CallArg::Named { name, value } => CallArg::Named {
                    name: name.clone(),
                    value: value.clone(),
                },
                CallArg::Slip(expr) => CallArg::Slip(expr.clone()),
                CallArg::Invocant(expr) => CallArg::Invocant(expr.clone()),
            })
            .collect()
    }

    /// Rewrite block values inside a hash literal to anonymous subs.
    /// Used for `is_run`'s expectation hash: `{ out => { ... } }`.
    pub(super) fn rewrite_hash_block_values(expr: &Expr) -> Expr {
        if let Expr::Hash(pairs, spelling) = expr {
            let rewritten_pairs = pairs
                .iter()
                .map(|(name, value)| {
                    let rewritten_value = value.as_ref().map(|v| {
                        if let Expr::Block(body) = v {
                            make_anon_sub(body.clone())
                        } else {
                            v.clone()
                        }
                    });
                    (name.clone(), rewritten_value)
                })
                .collect();
            Expr::Hash(rewritten_pairs, *spelling)
        } else {
            expr.clone()
        }
    }

    pub(super) fn has_phasers(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo | PhaserKind::First | PhaserKind::Next | PhaserKind::Last | PhaserKind::Pre | PhaserKind::Post)))
    }

    /// Check if a block body contains placeholder variables ($^a, $^b, etc.)
    /// that belong to the block itself — the same parameters
    /// `collect_placeholders_shallow` gives the closure it then compiles, so
    /// the two answers cannot disagree.
    // Cost: O(n log n), n = size of `stmts` (the collector sorts its names).
    pub(super) fn has_block_placeholders(stmts: &[Stmt]) -> bool {
        !crate::ast::collect_placeholders_shallow(stmts).is_empty()
    }

    /// A closure passed as a **method argument** escapes the creating frame.
    ///
    /// The callee decides whether the closure is invoked immediately or stored,
    /// and the caller cannot know which — `$reg.register({ $c++ })` keeps it
    /// alive long after the call returns. Raku closes over *containers*, so any
    /// captured-and-mutated local a stored closure names must be a shared
    /// `ContainerRef` cell; snapshotting it by value makes two closures over the
    /// same `my $c` disagree about its value (see
    /// `t/closure-arg-shares-its-captured-container.t`), which is the shape a
    /// Cro `route { my $i; get -> { $i++ } }` counter has.
    ///
    /// This used to be an allowlist (`then`/`tap`/`act`/`start`); everything else
    /// was classified non-escaping as a boxing-cost guard (#2746).
    pub(super) fn method_escapes_closure_args(_name: &str) -> bool {
        true
    }

    /// Whether an argument is a **closure literal** written at the call site.
    ///
    /// Only such an argument is compiled in an escaping position. Marking the
    /// whole argument list escaping instead costs 2.2x on
    /// `sub foo () { $ = 42 }; for ^2000000 { $ = foo }`
    /// (roast S04-declarations/state.t, which then times out under parallel
    /// load): `escaping_position` is read at EVERY closure-creation op compiled
    /// anywhere inside the argument expression, so a non-closure argument
    /// silently promoted unrelated inner blocks. The escape claim is about the
    /// literal the callee might store, and this is exactly that literal.
    pub(super) fn is_closure_literal_arg(arg: &Expr) -> bool {
        matches!(
            arg,
            Expr::Block(_)
                | Expr::AnonSub { .. }
                | Expr::AnonSubParams { .. }
                | Expr::Lambda { .. }
                // ADR-0033: a Whatever-curried argument (`.grep(* > $x)`) is a
                // closure literal written at the call site exactly like a
                // hand-written one — it just hasn't been expanded into its
                // `Lambda`/`AnonSubParams` form yet (that happens when this
                // argument itself gets compiled).
                | Expr::WhateverCurry(_)
        )
    }

    /// Strip a fat-arrow named-argument wrapper (`key => value`) down to the
    /// value expression, so a closure literal named argument (`now => { $x }`)
    /// is recognized by [`Self::is_closure_literal_arg`] the same way a positional
    /// one is. Every call-argument-escaping check must unwrap through this
    /// (not just re-match `Expr::Binary { op: FatArrow, .. }` locally) so the
    /// function-call and method-call compile paths cannot drift apart again.
    pub(super) fn unwrap_named_arg_value(arg: &Expr) -> &Expr {
        match arg {
            Expr::Binary {
                op: TokenKind::FatArrow,
                right,
                ..
            } => right.as_ref(),
            other => other,
        }
    }

    /// If `arg` is a direct private/public attribute read (`$!v`/`$.v`,
    /// compiled by `compile_expr` as a bare `Expr::Var`), follow the
    /// `GetLocal` it just emitted with [`OpCode::ResolveAttrRwCandidate`], so
    /// a runtime `MarkRwArgRefContext`/`MarkRwArgRefContextCallee` marker —
    /// emitted right after this call by `is_accessor_shaped_arg`-gated
    /// callers, landing between the two ops via `insert_accessor_ref_marker`
    /// — has an op to honor: an `is rw`/`is raw` callee parameter at this
    /// position then aliases the attribute's OWN shared cell, rather than
    /// losing (or, worse, misdirecting onto the callee's own same-named
    /// attribute) the writeback (#8904). Called right after `compile_expr(arg)`
    /// at every argument-compile site; a no-op for every other argument shape.
    ///
    /// Declines when the last op is not `GetLocal` (a `$.attr` public accessor
    /// compiles to a `CallMethod`, not a `Var` read — see `compile_expr_var` —
    /// so it is already covered by the accessor-call producer instead).
    pub(super) fn maybe_promote_attr_arg_read(&mut self, arg: &Expr) {
        let Expr::Var(name) = arg else { return };
        let Some((bare, _)) = crate::value::attr_twigil_base(name) else {
            return;
        };
        if !matches!(self.code.ops.last(), Some(OpCode::GetLocal(_))) {
            return;
        }
        let name_idx = self.code.add_constant(Value::str(bare.to_string()));
        self.code.emit(OpCode::ResolveAttrRwCandidate(name_idx));
    }

    /// Compile a method call argument.
    pub(super) fn compile_method_arg(&mut self, arg: &Expr) {
        self.compile_method_arg_with_escape(arg, false);
    }

    /// Like [`Self::compile_method_arg`] but lets the caller force the closure
    /// argument into an escaping position (for supply-consuming methods; see
    /// [`Self::method_escapes_closure_args`]).
    pub(super) fn compile_method_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // A method argument is normally passed to the callee, not stored in the
        // caller frame, so a closure argument is conservatively NON-escaping
        // (the #2746 guard). `tap`/`act` override this with `escaping = true`.
        // A non-bareword-keyed fat-arrow written directly as an argument
        // (`@a.push: "k$i" => $i`) parses to `PositionalPair` — it is *data*,
        // not a named argument, so the Pair it builds keeps its value's
        // container exactly like the standalone literal `my $p = ("k" => $v)`
        // does (S02:1704). `suppress_pair_capture` exists for the named-argument
        // case only (see its field doc); suppressing it here made every pushed
        // pair snapshot its value instead of aliasing it.
        let suppress_pairs = !matches!(arg, Expr::PositionalPair(_));
        self.with_escape(escaping, |s| {
            s.with_suppress_pair_capture(suppress_pairs, |s| {
                // An `AssignExpr` in argument position is always a real
                // assignment, evaluating to the assigned value
                // (`@r.push($x += 5)` pushes the assigned value, not a Pair).
                // This used to special-case a sigilless `name` as a named-arg
                // sugar (`foo(arg = 1)` -> `:arg(1)`), but raku itself rejects
                // a bareword assignment target as a parse error, and
                // `AssignExpr.name` never carries the `$` sigil for a genuine
                // scalar target (only `@`/`%` targets get one prepended) -- so
                // that check could never actually distinguish the two shapes
                // and instead misfired on every real `$x = ...`/`$x += ...`
                // argument. See todo/tickets/compound-assign-as-call-argument-yields-pair.md.
                // ADR-0021 I2/I3: a bareword-keyed fat-arrow (or colonpair,
                // same AST shape) written directly as this argument mints
                // the named-argument flavour, not the data default.
                if matches!(arg, Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow)
                {
                    s.mint_named_pair = true;
                }
                s.compile_expr(arg);
                s.maybe_promote_attr_arg_read(arg);
                if Self::needs_decont(arg) {
                    s.code.emit(OpCode::Decont);
                }
            })
        });
        // ADR-0021 (argument named-ness is a call-site property): named-ness
        // is decided by call-site syntax, not by what flavour of Pair the
        // argument expression happens to evaluate to. The function-call path
        // (`compile_call_arg_with_escape`, below) already normalizes every
        // non-syntactically-named argument at the call boundary; the method
        // path lacked this, so a Pair-valued variable/array-element/return
        // value leaked its named flavour straight into method dispatch
        // (`Pair.new($k,$v)` misbinding as a named arg, etc). Mirror the
        // function path here so both call kinds erase the flavour identically.
        if !Self::is_named_arg_expr(arg) {
            self.code.emit(OpCode::ContainerizePair);
        }
    }

    /// Check if an expression produces an array value that needs decontainerization
    /// for slurpy flattening at call sites.
    fn needs_decont(expr: &Expr) -> bool {
        match expr {
            Expr::ArrayVar(_) => true,
            // Assignment to @-variable returns an array
            Expr::AssignExpr { name, .. } => name.starts_with('@'),
            Expr::CompoundAssign { expanded, .. } => Self::needs_decont(expanded),
            // VarDecl/Assign in expression position (my @a = ...)
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => name.starts_with('@'),
                _ => false,
            },
            _ => false,
        }
    }

    /// A bind-index marker needs the same aggregate decontainerization for a
    /// block that returns an array/hash as it gets for a direct aggregate
    /// variable. Ordinary call arguments deliberately do not use this rule:
    /// `f(do { @a })` passes one item, while the marker's first argument is
    /// consumed as the value to install in an element and must retain the
    /// aggregate itself (`%h<k> := do { @a }`).
    fn bind_target_returns_aggregate(expr: &Expr) -> bool {
        match expr {
            Expr::ArrayVar(_) | Expr::HashVar(_) => true,
            Expr::Grouped(inner) => Self::bind_target_returns_aggregate(inner),
            Expr::Literal(value) => value.is_nil(),
            Expr::DoBlock { body, .. } => Self::bind_target_returns_aggregate_stmts(body),
            Expr::DoStmt(stmt) => Self::bind_target_returns_aggregate_stmt(stmt),
            Expr::Ternary {
                then_expr,
                else_expr,
                ..
            } => {
                Self::bind_target_returns_aggregate(then_expr)
                    && Self::bind_target_returns_aggregate(else_expr)
            }
            _ => false,
        }
    }

    fn bind_target_returns_aggregate_stmts(stmts: &[Stmt]) -> bool {
        crate::ast::last_value_stmt(stmts, crate::ast::TailSkip::Markers)
            .is_some_and(Self::bind_target_returns_aggregate_stmt)
    }

    fn bind_target_returns_aggregate_stmt(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(expr) => Self::bind_target_returns_aggregate(expr),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                Self::bind_target_returns_aggregate_stmts(body)
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                Self::bind_target_returns_aggregate_stmts(then_branch)
                    && (else_branch.is_empty()
                        || Self::bind_target_returns_aggregate_stmts(else_branch))
            }
            _ => false,
        }
    }

    /// Compile a function-call positional argument.
    /// Variable-like args are wrapped with source-name metadata so sigilless
    /// parameters (`\x`) can bind as writable aliases.
    pub(super) fn is_named_arg_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow => true,
            Expr::Literal(lit) if matches!(lit.view(), crate::value::ValueView::Pair(..)) => true,
            Expr::Unary { op, .. } if *op == crate::token_kind::TokenKind::Pipe => true,
            _ => false,
        }
    }

    pub(super) fn compile_call_arg(&mut self, arg: &Expr) {
        self.compile_call_arg_with_escape(arg, false);
    }

    /// Like `compile_call_arg` but lets the caller force the argument into an
    /// escaping position. Used for thread-spawning constructs (`start`) whose
    /// block argument genuinely outlives the call frame (it is stored in a
    /// Promise and run later on another thread), so the locals it captures and
    /// mutates must be promoted to shared `ContainerRef` cells (escape analysis).
    pub(super) fn compile_call_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // `f(++$p)` where `$p` is one of THIS routine's native `is rw`
        // parameters: `$p` is a native reference to the caller's location, and
        // rakudo's native `prefix:<++>` writes through it and hands the
        // reference back, so the callee's own `is rw` parameter binds the
        // caller's storage (`sub inner(int $p is rw) { --$p }` cancels the
        // increment two frames up). Compiling the increment as an ordinary
        // expression loses that: it leaves the incremented *value* on the
        // stack, and the callee rejects it as "a value without a container".
        //
        // Split it into the two things it means — perform the increment, then
        // pass the variable — so the argument takes the plain `Expr::Var`
        // path, which already binds a parameter through correctly. The gate in
        // `native_rw_param_incdec_operand` keeps every operand shape rakudo
        // rejects on the old value path, so those still error.
        if let Some(name) = self.native_rw_param_incdec_operand(arg) {
            self.compile_expr(arg);
            self.code.emit(OpCode::Pop);
            self.compile_call_arg_with_escape(&Expr::Var(name), escaping);
            return;
        }
        // `f($at-eos ?? $p !! ++$p)` — JSON::Fast's whitespace scanner. Each
        // arm of rakudo's conditional yields the native reference, so the
        // conditional as a whole IS that reference: whichever arm ran is what
        // binds. Compiling it as an ordinary expression would collapse both
        // arms to a value and lose the container, so compile the choice itself
        // into argument position — each arm through this same chokepoint, so
        // an arm spelled `++$p` still performs its increment and a bare `$p`
        // still binds the caller's storage. The arms may name different
        // parameters; only the one evaluated is bound.
        if let Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } = arg
            && self.is_native_rw_param_reference(then_expr)
            && self.is_native_rw_param_reference(else_expr)
        {
            self.compile_expr(cond);
            let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
            self.compile_call_arg_with_escape(then_expr, escaping);
            let jump_end = self.code.emit(OpCode::Jump(0));
            self.code.patch_jump(jump_else);
            self.compile_call_arg_with_escape(else_expr, escaping);
            self.code.patch_jump(jump_end);
            return;
        }
        // `f(@a[1] = v)`: perform the assignment, then pass the element itself
        // so an `is rw` parameter binds its container (see
        // `index_assign_arg_element`).
        if let Some(element) = Self::index_assign_arg_element(arg) {
            self.compile_expr(arg);
            self.code.emit(OpCode::Pop);
            self.compile_call_arg_with_escape(&element, escaping);
            return;
        }
        // Read-and-clear immediately: this call is the *direct* bind-target
        // compile iff the caller just set the flag for us. Clearing it up
        // front (before any nested `compile_expr`/`compile_call_arg`
        // recursion below) means a genuine call nested inside a bind RHS
        // (`my $x := f(@a[$i])`) sees `false` for its own argument compile,
        // so `f`'s `is rw` writeback machinery is untouched. See the field
        // doc on `bind_target_direct`.
        let is_bind_target = self.bind_target_direct;
        self.bind_target_direct = false;
        // One-shot: read and clear before any nested compilation, mirroring
        // `bind_target_direct` above, so a call nested inside this argument
        // does not inherit the suppression.
        let suppress_multidim_bind_ref = self.suppress_multidim_bind_ref_arg;
        self.suppress_multidim_bind_ref_arg = false;
        if is_bind_target
            && self.compile_bind_through_ternary(arg, escaping, suppress_multidim_bind_ref)
        {
            return;
        }
        if is_bind_target
            && let Expr::Var(name) = arg
            && name.starts_with("__mutsu_bind_index_assign_src_")
        {
            // A nested indexed assignment stores its source location in a
            // raw compiler temporary. Read that location without the ordinary
            // GetGlobal decontainerization before tagging it for the outer
            // bind.
            let name_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::GetCallTempRaw(name_idx));
            self.code.emit(OpCode::WrapVarRef {
                name_idx,
                slot: u32::MAX,
            });
            return;
        }
        // A multi-dimensional subscript (`@a[0;1;2]`, `%h{"a";"b"}`) passed as a
        // raw `\target` / `is rw` argument must alias the underlying nested
        // slot, so a later `target = v` inside the callee mutates the real
        // container and is visible immediately. Emit a `MultiDimIndexBindRef`
        // that descends to the leaf and promotes it to a shared `ContainerRef`
        // cell (a missing hash leaf gets a deferred `HashEntryRef`); the callee
        // binds through it. Slice dimensions that can't collapse to one cell
        // yield a list of leaf cells, or fall back to the plain read value.
        //
        // Suppressed for the synthetic `__mutsu_list_assign_rhs` helper's
        // argument (see `suppress_multidim_bind_ref_arg`): that call is a
        // native value-only deitemizer, not a routine with a raw/`is rw`
        // parameter, so its argument must be a plain read.
        if !suppress_multidim_bind_ref
            && let Expr::MultiDimIndex {
                target,
                dimensions,
                is_positional,
            } = arg
        {
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.code.emit(OpCode::MultiDimIndexBindRef {
                ndims: dimensions.len() as u32,
                is_positional: *is_positional,
            });
            return;
        }
        // Inside a `return-rw` operand a single-dimension subscript argument is
        // part of the lvalue chain and must alias the element's container, the
        // same way the multi-dim form above always does.
        if self.rw_return_operand && matches!(arg, Expr::Index { .. }) {
            self.compile_rw_chain_index_arg(arg);
            return;
        }
        // A call argument's value is normally passed to the callee, not stored
        // in the caller frame, so a closure argument is conservatively
        // NON-escaping (the #2746 guard: `map {...}` / `lives-ok {...}` must not
        // box even when the whole call sits in an escaping position like
        // `my @r = map {...}`). `start` overrides this with `escaping = true`.
        // ADR-0021 I2/I3: a bareword-keyed fat-arrow (or colonpair, same AST
        // shape) written directly as this argument mints the named-argument
        // flavour, not the data default.
        if matches!(arg, Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow) {
            self.mint_named_pair = true;
        }
        // See `compile_method_arg_with_escape`: a `PositionalPair` argument is
        // data, not a named argument, so its value keeps its container.
        // An inline scalar declaration passed directly to a call must have a
        // lexical slot. Its VarRef then identifies a writable container even
        // when the initial value is the Any type object (`h(my $z)`). Keep the
        // request scoped to this argument; other expression declarations use
        // their existing env-only path.
        let call_arg_decl = if let Expr::DoStmt(stmt) = arg
            && let Stmt::VarDecl {
                name,
                is_our: false,
                custom_traits,
                ..
            } = stmt.as_ref()
            && !name.starts_with(['@', '%', '&'])
            && !name.starts_with("__ANON")
            && !custom_traits
                .iter()
                .any(|(trait_name, _)| trait_name == "__constant")
            && !self.promoted_expr_decl_names.contains(name)
        {
            Some(name.clone())
        } else {
            None
        };
        let inserted = call_arg_decl
            .as_ref()
            .is_some_and(|name| self.call_arg_decl_slots.insert(name.clone()));
        let suppress_pairs = !matches!(arg, Expr::PositionalPair(_));
        self.with_escape(escaping, |c| {
            c.with_suppress_pair_capture(suppress_pairs, |c| c.compile_expr(arg))
        });
        if inserted && let Some(name) = call_arg_decl.as_ref() {
            self.call_arg_decl_slots.remove(name);
        }
        self.maybe_promote_attr_arg_read(arg);
        if Self::needs_decont(arg) || (is_bind_target && Self::bind_target_returns_aggregate(arg)) {
            self.code.emit(OpCode::Decont);
        }
        if !Self::is_named_arg_expr(arg) {
            self.code.emit(OpCode::ContainerizePair);
        }
        // A sigil-less constant's binding is its term key (#9962). An
        // anonymous scalar assignment (`$ = value`) produces a writable
        // container, so it is wrapped with VarRef so `is rw` dispatch can
        // match. An inline declaration used as an argument (`$y := my $x`,
        // `f(my $z)`) parses to `DoStmt(VarDecl { .. })`: compiling it
        // declares the variable in the enclosing scope and leaves its value on
        // the stack, and the VarRef to the freshly-declared variable lets a
        // `:=` bind (or an `is rw` parameter) alias the new container rather
        // than snapshot its value.
        let source_name = match arg {
            Expr::CodeVar(_) => arg.var_key(),
            // A bareword this frame does not know as a variable compiles to a
            // `GetBareWord` term lookup: it has no container to tag, and a tag
            // under its bare spelling would let `WrapVarRef` swap in whatever
            // a package store holds under that name -- a class body's nested
            // type `Q::Atom` in place of its `my constant Atom` (#11385).
            Expr::BareWord(name) if !self.bareword_denotes_variable(name) => None,
            _ => self.lvalue_root_key(
                arg,
                crate::ast::LvaluePeel::ASSIGN
                    | crate::ast::LvaluePeel::DECL
                    | crate::ast::LvaluePeel::SIGILLESS,
            ),
        };
        if matches!(arg, Expr::Index { .. }) && is_bind_target {
            // `:=` bind to an Index expression (`my $x := @a[$i]`): the Index
            // compile already promoted the element to a first-class
            // `ContainerRef` cell on the stack (IndexAutovivifyLazyTerminal /
            // array_slot_ref). Just wrap it with VarRef so SetLocal's
            // `extract_varref_binding` sees `is_bind = true`.
            //
            // An ordinary subscript ARGUMENT gets nothing here: it compiles to
            // a plain `Index`, which the call emitter swaps for
            // `IndexArgRef` so a callee that binds the caller's container
            // receives the element's own location (ADR-0059 Slice 3, which
            // retired the copy-in/copy-out `__mutsu_index_rw_arg_*` temps).
            let tmp = format!("__mutsu_bind_index_ref_{}", self.code.constants.len());
            let name_idx = self.code.add_constant(Value::str(tmp));
            self.code.emit(OpCode::WrapVarRef {
                name_idx,
                slot: u32::MAX,
            });
        } else if let Some(name) = source_name {
            // Deliberately the non-ADR-0032-D1 emitter here, for EVERY arg
            // shape (including a genuine `Expr::Var`/`Expr::DoStmt` VarDecl
            // read). This call site fires for every plain call argument in
            // the language (not only an `is rw`/`:=`-bound one) purely to
            // tag its shape for LATER is-rw dispatch matching — unlike the
            // narrow, deliberate WrapVarRef sites (fat-arrow value, Pair.new
            // value-arg, Capture item, list-literal element, meta-identity
            // operand), it is not itself a container-capture-semantics site.
            // Registering it anyway is not just imprecise for a bareword
            // (`emit_wrap_var_ref_arg_tag`'s doc comment) — it also over-
            // boxes a genuine free-variable argument passed to an ordinary
            // (non-`is rw`) function: `t/hash-attr-map-default-element-
            // assign.t` broke because `lives-ok { $c.h{3} = Str }` compiles
            // `$c` as an rw-tagged argument to an internal hash-element-
            // assign helper, and boxing `$c`'s OWN declaration into a
            // `ContainerRef` cell (Half A) corrupted class-instance
            // attribute-hash access through it. Probes `V`/`W` (an `is rw`
            // argument / `:=` bind performed inside a closure) do not need
            // D1 to pass — they already work through the pre-existing
            // `free_var_writes` write-tracking machinery (ADR-0032 §1.4).
            self.emit_wrap_var_ref_arg_tag(&name);
        } else if is_bind_target
            && matches!(
                arg,
                Expr::MethodCall { .. } | Expr::DynamicMethodCall { .. }
            )
        {
            // `:=` bind to a method-call RHS (`my $ref := $obj.attr`): flag the
            // dispatch so a public attribute accessor returns the attribute
            // slot's `ContainerRef` cell instead of a value copy — the bound
            // variable then aliases the attribute container (writes through
            // either side are seen by both). A non-accessor method ignores the
            // flag and the bind degrades to today's bind-by-value.
            self.mark_trailing_method_call_as_accessor_ref();
        }
    }

    /// Insert a `MarkAccessorRefContext` immediately before the trailing
    /// `CallMethod`/`CallMethodMut` op (skipping the post-call `Decont` /
    /// `ContainerizePair` the arg compile may have appended), so that ONE
    /// dispatch sees the accessor-ref flag. Inserting (rather than emitting
    /// after the fact) is safe here: any jump patched to the call op's old
    /// index now lands on the marker and falls through to the same call.
    /// No-op when the compiled tail is not a method call.
    pub(super) fn mark_trailing_method_call_as_accessor_ref(&mut self) {
        self.insert_accessor_ref_marker(OpCode::MarkAccessorRefContext);
    }

    /// The same insertion, with the runtime-gated marker
    /// (`MarkLvalueInvocantRefContext`) ADR-0067's E6 producer emits before an
    /// lvalue method call's *invocant*. `method_name` is the OUTER method's
    /// name — the one whose invocant this is — when it is a compile-time
    /// literal, and `None` for the dynamic spelling. See that opcode's doc
    /// comment: the compiler cannot know whether that callee binds its invocant
    /// raw, so the marker is emitted for every `$obj.acc.m = v` and the VM
    /// declines it unless a raw-invocant callee is possible for that name.
    pub(super) fn mark_trailing_method_call_as_lvalue_invocant_ref(
        &mut self,
        method_name: Option<&str>,
    ) {
        let idx = method_name.map(|n| self.code.add_constant(Value::str(n.to_string())));
        self.insert_accessor_ref_marker(OpCode::MarkLvalueInvocantRefContext(idx));
    }

    /// ADR-0067's argument producer: ask a positional argument that is an
    /// attribute-accessor read to hand back the attribute's *container*, so an
    /// `is rw` / `is raw` / sigil-less parameter of `callee` binds the caller's
    /// location instead of a value copy.
    ///
    /// The marker is runtime-gated on the callee's declaration
    /// (`OpCode::MarkRwArgRefContext`), because the compiler cannot know it — a
    /// routine may be declared after its use site. No-op when the compiled tail
    /// is not a method call, or when the argument shape could not be an
    /// accessor read anyway (an argument-carrying call is never one).
    pub(super) fn mark_arg_as_rw_container_candidate(
        &mut self,
        callee: &str,
        positional: u32,
        arg: &Expr,
    ) {
        if !Self::is_accessor_shaped_arg(arg) {
            return;
        }
        // A `__mutsu_*` helper is not a user routine and has no registered
        // signature to gate on, so a marker naming one could only ever answer
        // "no". The two helpers whose *real* callee is a string argument reach
        // this through `relayed_rw_arg_callee`, which passes that real name.
        if callee.starts_with("__mutsu_") {
            return;
        }
        let callee_idx = self.code.add_constant(Value::str(callee.to_string()));
        self.insert_accessor_ref_marker(OpCode::MarkRwArgRefContext {
            callee_idx,
            positional,
        });
    }

    /// ADR-0067's argument producer for a callee with **no compile-time name**
    /// — a method call, or a call through a code value. Same intent as
    /// [`Self::mark_arg_as_rw_container_candidate`], different gate: there is
    /// no name to look up, so the marker records where the callee itself sits
    /// on the stack and the VM asks that callee's own signature. See
    /// [`OpCode::MarkRwArgRefContextCallee`].
    pub(super) fn mark_arg_as_rw_container_candidate_callee(
        &mut self,
        callee: crate::opcode::RwArgCallee,
        positional: Option<u32>,
        stack_offset: u32,
        arg: &Expr,
    ) {
        let Some(positional) = positional else {
            return;
        };
        if !Self::is_accessor_shaped_arg(arg) {
            return;
        }
        self.insert_accessor_ref_marker(OpCode::MarkRwArgRefContextCallee(Box::new(
            crate::opcode::RwArgCalleeMark {
                positional,
                stack_offset,
                callee,
            },
        )));
    }

    /// ADR-0067's subscript-ARGUMENT producer: swap the just-compiled
    /// argument's trailing `Index` for [`OpCode::IndexArgRef`], so `$b(@a[0])`
    /// / `$obj.m(@a[0])` / `&g(@a[0])` can hand the element's own container to a
    /// callee that binds that argument to the caller's location.
    ///
    /// A *replacement* rather than an inserted marker, for the same reason
    /// [`Self::mark_trailing_index_as_invocant_ref`] is one: the location has to
    /// be produced by the subscript itself — once `Index` has run, the element's
    /// value is on the stack and nothing can reach back for its slot.
    ///
    /// No-op unless the argument really is a subscript whose compiled tail is a
    /// plain `Index`. The mutating/autovivifying subscript emitters
    /// (`IndexElemAutoviv`, `IndexAutovivifyLazy`) are left alone: they hand
    /// back a shared node or a deferred path, which is a different contract.
    pub(super) fn mark_arg_index_as_container_candidate_callee(
        &mut self,
        callee: crate::opcode::RwArgCallee,
        positional: Option<u32>,
        stack_offset: u32,
        arg: &Expr,
    ) {
        let Some(positional) = positional else {
            return;
        };
        if !matches!(arg, Expr::Index { .. }) {
            return;
        }
        // Skip back over the post-read ops the argument compile may have
        // appended, the same way `insert_accessor_ref_marker` does — the method
        // arg emitter puts a `ContainerizePair` after the subscript.
        let mut last = self.code.ops.len();
        while last > 0 {
            match self.code.ops[last - 1] {
                OpCode::Decont | OpCode::ContainerizePair => last -= 1,
                _ => break,
            }
        }
        if last == 0 {
            return;
        }
        last -= 1;
        let OpCode::Index { is_positional } = self.code.ops[last] else {
            return;
        };
        let line = self.code.op_lines[last];
        self.code.ops[last] = OpCode::IndexArgRef(Box::new(crate::opcode::IndexArgRefMark {
            mark: crate::opcode::RwArgCalleeMark {
                positional,
                stack_offset,
                callee,
            },
            is_positional,
        }));
        self.code.op_lines[last] = line;
    }

    /// ADR-0059 Slice 3: compile one argument of a *named* routine call
    /// (`g(@a[0])`, or a user-defined infix operator's operand), whose callee
    /// is looked up by name at run time ([`crate::opcode::RwArgCallee::Named`]).
    /// `callee` is `None` when no subscript producer applies, and the argument
    /// is then compiled exactly as [`Self::compile_call_arg_with_escape`] does.
    ///
    /// A single-level subscript compiles to a plain `Index` swapped for
    /// ADR-0067's `IndexArgRef`, which hands over the element's location when a
    /// candidate of the callee binds that positional to the caller's container.
    ///
    /// A *nested* subscript (`g(%h<a><b>)`) cannot be answered by one op: the
    /// inner `%h<a>` has already been read as a value by the time the last
    /// subscript runs, and a missing intermediate level reads as `Any`, which
    /// has no location. So the same gate is asked up front
    /// ([`OpCode::RwArgCalleeBindsContainer`]) and the argument is compiled
    /// twice: the ordinary read, and the `return-rw` operand's container-mode
    /// chain ([`Self::compile_rw_chain_index_arg`]), whose missing levels are
    /// the deferred vivification token — so binding creates nothing, and the
    /// first write through the parameter creates the whole path (roast
    /// `S02-types/autovivification.t`).
    ///
    /// Any index expression is accepted, computed ones included
    /// (`g(%h{@k[$i++]}<z>)`, #10044): whether a level turns out to be a slice,
    /// a `Junction` or a `WhateverCode` is a run-time fact, so the chain's
    /// lazy ops decline to an ordinary read for such an index themselves.
    ///
    /// `positional` is the argument's entry of [`Self::arg_positional_indices`];
    /// `None` there means either a named argument (never marked: a named
    /// parameter is not what this gate reads) or an argument after a `|slip`,
    /// whose signature index is only known at run time and is asked as
    /// [`crate::opcode::RWARG_POSITIONAL_UNKNOWN`].
    pub(super) fn compile_named_callee_arg(
        &mut self,
        callee: Option<&str>,
        positional: Option<u32>,
        arg: &Expr,
        escaping: bool,
    ) {
        // `f(@a[1] = v)`: assign first, then treat the element as the argument
        // so the container-candidate marking below sees an `Expr::Index`.
        if let Some(element) = Self::index_assign_arg_element(arg) {
            self.compile_expr(arg);
            self.code.emit(OpCode::Pop);
            self.compile_named_callee_arg(callee, positional, &element, escaping);
            return;
        }
        let callee = callee.filter(|callee| {
            matches!(arg, Expr::Index { .. })
                && !Self::index_arg_is_static_slice(arg)
                && !Self::is_named_arg_expr(arg)
                // A `__mutsu_*` helper is not a user routine and has no
                // registered signature, so the gate could only answer "no".
                && !callee.starts_with("__mutsu_")
        });
        let Some(callee) = callee else {
            self.compile_call_arg_with_escape(arg, escaping);
            return;
        };
        let name_idx = self.code.add_constant(Value::str(callee.to_string()));
        let mark = crate::opcode::RwArgCalleeMark {
            positional: positional.unwrap_or(crate::opcode::RWARG_POSITIONAL_UNKNOWN),
            stack_offset: 0,
            callee: crate::opcode::RwArgCallee::Named { name_idx },
        };
        let nested = matches!(arg, Expr::Index { target, .. } if matches!(target.as_ref(), Expr::Index { .. }));
        if !nested {
            self.compile_call_arg_with_escape(arg, escaping);
            self.mark_arg_index_as_container_candidate_callee(
                mark.callee,
                Some(mark.positional),
                0,
                arg,
            );
            return;
        }
        self.code
            .emit(OpCode::RwArgCalleeBindsContainer(Box::new(mark)));
        // `JumpIfTrue` only PEEKS its condition (it is `||`'s short-circuit),
        // which leaked the gate's Bool under the argument: a block whose value
        // is the call (`.map({ slip(@a[1][0]) })`) answered `False`. Negate it
        // and branch on the popping `JumpIfFalse` instead.
        self.code.emit(OpCode::Not);
        let to_container = self.code.emit(OpCode::JumpIfFalse(0));
        // The ordinary read first, so it is the compile that consumes the
        // one-shot argument flags `compile_call_arg_with_escape` reads.
        self.compile_call_arg_with_escape(arg, escaping);
        let to_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(to_container);
        // An immutable `List`'s element is handed over raw, as a `return-rw`
        // operand's is: the parameter's writability then follows the element
        // (a `List` of values is not a set of containers), and the `List`
        // itself is never mutated to hold a promoted cell.
        let saved_raw_list_elem = self.raw_list_elem_terminal;
        self.raw_list_elem_terminal = true;
        self.compile_rw_chain_index_arg(arg);
        self.raw_list_elem_terminal = saved_raw_list_elem;
        self.code.patch_jump(to_end);
    }

    /// The signature-positional index of each syntactic argument, or `None`
    /// where there is none to name: a named argument (`:k(v)` / `k => v`)
    /// consumes no positional slot, and a `|EXPR` slip spreads an unknown
    /// number of them, so every argument after one has no compile-time index at
    /// all. Used by ADR-0067's argument producers, which must name the callee's
    /// parameter, not the argument list's own offset.
    pub(super) fn arg_positional_indices(args: &[Expr]) -> Vec<Option<u32>> {
        let mut next = 0u32;
        let mut unknown = false;
        args.iter()
            .map(|arg| {
                if unknown || Self::is_named_arg_expr(arg) {
                    // A slip makes every later position unknowable; a plain
                    // named argument only skips itself.
                    unknown |= matches!(arg, Expr::Unary { op, .. }
                        if *op == crate::token_kind::TokenKind::Pipe);
                    return None;
                }
                let idx = next;
                next += 1;
                Some(idx)
            })
            .collect()
    }

    /// `@a.AT-POS(EXPR)`: a plain one-argument call on an `@` variable (not a
    /// scalar, which may hold a class with its own `AT-POS`), the positional
    /// subscript `@a[EXPR]` spelled as a
    /// method.
    pub(super) fn is_array_at_pos_call(arg: &Expr) -> bool {
        matches!(
            arg,
            Expr::MethodCall {
                target,
                name,
                args,
                modifier: None,
                quoted: false,
            } if args.len() == 1
                && matches!(**target, Expr::ArrayVar(_))
                && name.with_str(|n| n == "AT-POS")
        )
    }

    /// The `Expr::Index` spelling of an [`Self::is_array_at_pos_call`] argument;
    /// any other expression is returned unchanged.
    pub(super) fn at_pos_call_as_index(arg: &Expr) -> Expr {
        match arg {
            Expr::MethodCall { target, args, .. } if Self::is_array_at_pos_call(arg) => {
                Expr::Index {
                    target: target.clone(),
                    index: Box::new(args[0].clone()),
                    is_positional: true,
                }
            }
            other => other.clone(),
        }
    }

    /// Whether an argument expression *could* be a public attribute accessor
    /// read (`$c.v`) or a direct private/public attribute read (`$!v`/`$.v`
    /// compiled as a bare `Expr::Var`) — the two shapes whose compiled
    /// bytecode can answer a pending rw-container marker with the attribute's
    /// shared container instead of a value copy (`try_fast_accessor_read`'s
    /// `want_ref` branch for the first shape, `exec_resolve_attr_rw_candidate_op`
    /// for the second). Keeping the test here (rather than leaving it to the
    /// runtime) is what stops the marker being emitted, and its callee lookup
    /// executed, for the overwhelming majority of call arguments.
    pub(super) fn is_accessor_shaped_arg(arg: &Expr) -> bool {
        matches!(
            arg,
            Expr::MethodCall {
                args,
                modifier: None,
                quoted: false,
                ..
            } if args.is_empty()
        ) || matches!(arg, Expr::Var(name) if crate::value::attr_twigil_base(name).is_some())
    }

    fn insert_accessor_ref_marker(&mut self, marker: OpCode) {
        let mut i = self.code.ops.len();
        while i > 0 {
            match &self.code.ops[i - 1] {
                // `WrapVarRef` is skippable alongside `Decont`/`ContainerizePair`
                // for the `ResolveAttrRwCandidate` anchor below: every
                // `$!attr`/`$.attr` call argument gets tagged with it
                // (`emit_wrap_var_ref_arg_tag`, for rw-source tracking
                // regardless of container-candidacy), so it can sit between
                // that op and the trailing call for that shape (a plain-sub
                // call site emits it; a method-call site does not). It is
                // never present after a bare accessor-call argument (`$c.v`)
                // — `positional_arg_source_name` does not match
                // `Expr::MethodCall` — so skipping it here cannot walk past an
                // unrelated `WrapVarRef` belonging to a different argument.
                OpCode::Decont | OpCode::ContainerizePair | OpCode::WrapVarRef { .. } => i -= 1,
                OpCode::CallMethod { .. }
                | OpCode::CallMethodMut { .. }
                | OpCode::CallMethodDynamic { .. }
                | OpCode::CallMethodDynamicMut { .. }
                | OpCode::ResolveAttrRwCandidate(..) => {
                    // Keep the ip -> line table (`op_lines`) aligned with `ops`:
                    // the marker inherits the call's line.
                    let line = self.code.op_lines[i - 1];
                    self.code.ops.insert(i - 1, marker);
                    self.code.op_lines.insert(i - 1, line);
                    return;
                }
                _ => return,
            }
        }
    }

    /// ADR-0067's subscript-receiver producer: swap the just-compiled receiver's
    /// trailing `Index` for [`OpCode::IndexInvocantRef`], so `@a[0].mut` /
    /// `%h<a>.mut` can hand the element's own container to a callee that binds
    /// its invocant raw.
    ///
    /// A *replacement* rather than an inserted marker, because the location has
    /// to be produced by the subscript itself: unlike the E6 accessor producer,
    /// there is no later op that could still reach back for it — `Index` has
    /// already read the element's value out of the container.
    ///
    /// No-op unless the receiver really is a subscript whose compiled tail is a
    /// plain `Index`. The mutating subscript-method path (`@a[0].push`) compiles
    /// to `IndexElemAutoviv` instead and is left alone: it hands back the
    /// element's shared node for in-place mutation, which is a different
    /// contract from replacing the element.
    pub(super) fn mark_trailing_index_as_invocant_ref(&mut self, target: &Expr) {
        if !matches!(target, Expr::Index { .. }) {
            return;
        }
        if let Some(last) = self.code.ops.last_mut()
            && let OpCode::Index { is_positional } = *last
        {
            *last = OpCode::IndexInvocantRef { is_positional };
        }
    }

    /// True when `arg` is a subscript whose index is *statically* a slice — a
    /// range (`@a[1..*]`, `@a[0..^2]`), a sequence (`@a[1...*]`), a bare
    /// `*`/`**`, or a literal index list (`@a[0,1]`, `%h<a b>`).
    ///
    /// Such a subscript yields a list of values rather than one element's
    /// container, so it can never bind to an `is rw` parameter (rakudo:
    /// `X::Parameter::RW`), and the named-call emitter leaves it a plain
    /// `Index` instead of an `IndexArgRef` whose gate could only decline it.
    /// Only the shapes visible in the AST are recognized: a subscript whose
    /// index only turns out to be a Range at runtime (`@a[$r]`) reaches the
    /// producer, which declines any non-integer index at run time.
    pub(super) fn index_arg_is_static_slice(arg: &Expr) -> bool {
        let Expr::Index { index, .. } = arg else {
            return false;
        };
        match index.as_ref() {
            Expr::Binary { op, .. } => matches!(
                op,
                TokenKind::DotDot
                    | TokenKind::DotDotCaret
                    | TokenKind::CaretDotDot
                    | TokenKind::CaretDotDotCaret
                    | TokenKind::DotDotDot
                    | TokenKind::DotDotDotCaret
            ),
            Expr::Whatever | Expr::HyperWhatever | Expr::ArrayLiteral(_) => true,
            _ => false,
        }
    }

    /// Check for placeholder variable conflicts in a block/sub body.
    /// Returns a Value to die with if a conflict is found.
    /// `decl_kind` is Some("sub") for named subs, None for blocks.
    pub(super) fn check_placeholder_conflicts(
        &self,
        params: &[String],
        body: &[Stmt],
        decl_kind: Option<&str>,
    ) -> Option<Value> {
        use crate::ast::has_var_decl;
        use crate::placeholder_order::{
            bare_name_shadowed_by_nested_placeholder, bare_precedes_placeholder,
        };
        for param in params {
            let bare_name = if let Some(b) = param.strip_prefix("&^") {
                b
            } else if let Some(b) = param.strip_prefix('^') {
                b
            } else {
                continue;
            };
            // Check for `my $name` in the same scope → X::Redeclaration.
            // rakudo names the placeholder as the redeclared symbol and says
            // what kind of redeclaration it is in `.postfix`, which the message
            // repeats ("Redeclaration of symbol '$^foo' as a placeholder
            // parameter."). Spelling this as a bare `"X::Type: text"` string
            // instead lost the class: `$!` saw an `X::AdHoc`, and only the
            // native `throws-like`'s message sniffing kept
            // `roast/S06-signature/positional-placeholders.t` green.
            if has_var_decl(body, bare_name) {
                let symbol = format!("$^{}", bare_name);
                let postfix = "as a placeholder parameter";
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
                attrs.insert("what".to_string(), Value::str("symbol".to_string()));
                attrs.insert("postfix".to_string(), Value::str(postfix.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Redeclaration of symbol '{symbol}' {postfix}.")),
                );
                return Some(Value::make_instance(
                    Symbol::intern("X::Redeclaration"),
                    attrs,
                ));
            }
            // Check if bare var precedes placeholder in the body
            if bare_precedes_placeholder(body, bare_name) {
                // If outer scope has this variable → X::Placeholder::NonPlaceholder
                if self.local_map.contains_key(bare_name) {
                    let decl = decl_kind.unwrap_or("block");
                    let message = format!(
                        "'${}' has already been used as a non-placeholder in the surrounding {}, \
                         so you will confuse the reader if you suddenly declare $^{} here",
                        bare_name, decl, bare_name
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "variable_name".to_string(),
                        Value::str(format!("${}", bare_name)),
                    );
                    attrs.insert(
                        "placeholder".to_string(),
                        Value::str(format!("$^{}", bare_name)),
                    );
                    attrs.insert("decl".to_string(), Value::str(decl.to_string()));
                    attrs.insert("message".to_string(), Value::str(message));
                    return Some(Value::make_instance(
                        Symbol::intern("X::Placeholder::NonPlaceholder"),
                        attrs,
                    ));
                } else {
                    // No outer declaration → X::Undeclared. Verified against
                    // `raku`: a bare `$name` preceding its own `$^name` does
                    // NOT get a "Did you mean" suggestion (the placeholder is
                    // not a candidate the suggestion mechanism considers) —
                    // it falls to the same default message as the
                    // nested-placeholder-shadow case below.
                    let symbol = format!("${}", bare_name);
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("name".to_string(), Value::str(symbol.clone()));
                    attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
                    attrs.insert("post".to_string(), Value::str(symbol.clone()));
                    attrs.insert("highexpect".to_string(), Value::array(vec![]));
                    attrs.insert("suggestions".to_string(), Value::array(vec![]));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Variable '{}' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
                            symbol
                        )),
                    );
                    return Some(Value::make_instance(Symbol::intern("X::Undeclared"), attrs));
                }
            }
        }
        // A bare `$name` used in THIS block's own scope, where `$^name` is
        // declared only by a block STRICTLY NESTED inside this one (a nested
        // `if`/`for`/`given` BLOCK body, `whenever`, or closure) — e.g.
        // `{ for 1 { $^b }; say $b }`. The inner block owns that placeholder;
        // it does not make `$b` this block's parameter, so `$b` here was
        // simply never declared — the same generic X::Undeclared rakudo
        // raises for any undeclared bare variable (unrelated to the nested
        // `$^name`, which is why the message does not mention it).
        if let Some(bare_name) = bare_name_shadowed_by_nested_placeholder(body, params)
            && !has_var_decl(body, &bare_name)
            && !self.local_map.contains_key(bare_name.as_str())
            // `bare_name` may also be legitimately declared as THIS block's own
            // (non-placeholder) signature parameter, e.g. `-> $b, $i { ... }`
            // where a totally separate nested closure happens to use `$^b` —
            // that inner closure's placeholder does not conflict with the
            // outer pointy block's own `$b` (see
            // `t/placeholder-nested-block-scope.t`'s "bitwise placeholder
            // blocks, slipped arguments" case).
            && !params.iter().any(|p| p == &bare_name)
        {
            let symbol = format!("${}", bare_name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(symbol.clone()));
            attrs.insert("symbol".to_string(), Value::str(symbol.clone()));
            attrs.insert("post".to_string(), Value::str(symbol.clone()));
            attrs.insert("highexpect".to_string(), Value::array(vec![]));
            attrs.insert("suggestions".to_string(), Value::array(vec![]));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Variable '{}' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?",
                    symbol
                )),
            );
            return Some(Value::make_instance(Symbol::intern("X::Undeclared"), attrs));
        }
        None
    }

    /// Whether a parameter's declared type is one of Raku's *native* types —
    /// the ones whose storage is a machine value rather than a `Scalar`, so an
    /// `is rw` parameter of that type binds a native reference.
    pub(crate) fn is_native_type_constraint(&self, constraint: &str) -> bool {
        let constraint = self.resolve_type_alias_constraint(constraint);
        let constraint = constraint
            .strip_suffix(":D")
            .or_else(|| constraint.strip_suffix(":U"))
            .or_else(|| constraint.strip_suffix(":_"))
            .unwrap_or(&constraint);
        crate::runtime::native_types::is_native_int_type(constraint)
            || matches!(constraint, "num" | "num32" | "num64" | "str")
    }

    /// Record this routine's native-typed `is rw` parameters, which
    /// [`Compiler::native_rw_param_incdec_operand`] gates on. See the field
    /// doc on `native_rw_params`.
    /// Record each native-integer parameter's declared type in `local_types`,
    /// as a `my int $x` declaration records its own, so the arithmetic on it
    /// takes the same native (wrapping) operation a native variable gets:
    /// `sub f(int $a) { $a + 1 }` wraps at the int64 edge in rakudo and in
    /// TRIR, and the untyped path must agree with both (#9270). Only native
    /// integer types are seeded; a boxed parameter type would feed the
    /// compile-time literal checks that read the same map.
    pub(crate) fn seed_native_int_param_types(&mut self, param_defs: &[crate::ast::ParamDef]) {
        for pd in param_defs {
            if pd.name.is_empty() || pd.slurpy || pd.double_slurpy {
                continue;
            }
            if let Some(tc) = pd.type_constraint.as_deref()
                && self.is_native_type_constraint(tc)
            {
                self.local_types
                    .insert(pd.name.clone(), self.resolve_type_alias_constraint(tc));
            }
        }
    }

    pub(crate) fn seed_native_rw_params(&mut self, param_defs: &[crate::ast::ParamDef]) {
        self.native_rw_params = param_defs
            .iter()
            .filter(|pd| {
                !pd.name.is_empty()
                    && pd
                        .type_constraint
                        .as_deref()
                        .is_some_and(|tc| self.is_native_type_constraint(tc))
                    && pd.traits.iter().any(|t| t == "rw")
            })
            .map(|pd| pd.name.clone())
            .collect();
    }

    /// A call argument that still denotes one of the enclosing routine's
    /// native `is rw` parameters after being evaluated — `++$p` / `--$p`, or a
    /// conditional whose arms both do — which is what rakudo lets bind through
    /// to another native `is rw` parameter. Returns the parameter's name.
    ///
    /// Deliberately NOT matched: the postfix forms (`$p++` yields the old
    /// value, and rakudo rejects it here with "Expected a modifiable native
    /// int argument"), and any operand that is not such a parameter (a plain
    /// `my int $x`, or a non-native `$p is rw`, both of which rakudo also
    /// rejects). Keeping those on the existing value path preserves the error.
    pub(super) fn native_rw_param_incdec_operand(&self, arg: &Expr) -> Option<String> {
        let Expr::Unary { op, expr } = arg else {
            return None;
        };
        if !matches!(
            op,
            crate::token_kind::TokenKind::PlusPlus | crate::token_kind::TokenKind::MinusMinus
        ) {
            return None;
        }
        let Expr::Var(name) = expr.as_ref() else {
            return None;
        };
        self.native_rw_params.contains(name).then(|| name.clone())
    }

    /// Does this argument sub-expression still denote a native `is rw`
    /// parameter after it is evaluated? True for the bare parameter and for a
    /// prefix increment/decrement of one — the two shapes rakudo keeps as a
    /// reference. Used to decide whether a conditional argument
    /// ([`Compiler::compile_call_arg_with_escape`]) can bind through.
    pub(super) fn is_native_rw_param_reference(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Var(name) => self.native_rw_params.contains(name),
            _ => self.native_rw_param_incdec_operand(expr).is_some(),
        }
    }

    /// Check for assignment to native-typed read-only parameters inside a
    /// sub/method/block body. Returns an X::Assignment::RO::Comp error value
    /// if such an assignment is found.
    pub(crate) fn check_native_readonly_param_assignment(
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> Option<Value> {
        // Build set of native-typed param names that are NOT `is rw` or `is copy`
        let readonly_native_params: std::collections::HashSet<&str> = param_defs
            .iter()
            .filter(|pd| {
                let is_native = pd.type_constraint.as_deref().is_some_and(|tc| {
                    crate::runtime::native_types::is_native_int_type(tc)
                        || matches!(tc, "num" | "num32" | "num64" | "str")
                });
                let has_rw_or_copy = pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                is_native && !has_rw_or_copy
            })
            .map(|pd| pd.name.as_str())
            .collect();
        if readonly_native_params.is_empty() {
            return None;
        }
        fn scan_stmts(
            stmts: &[Stmt],
            readonly: &std::collections::HashSet<&str>,
        ) -> Option<String> {
            for stmt in stmts {
                if let Some(name) = scan_stmt(stmt, readonly) {
                    return Some(name);
                }
            }
            None
        }
        fn scan_stmt(stmt: &Stmt, readonly: &std::collections::HashSet<&str>) -> Option<String> {
            match stmt {
                Stmt::Assign { name, .. } => {
                    if readonly.contains(name.as_str()) {
                        return Some(format!("${}", name));
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if let Some(n) = scan_stmts(then_branch, readonly) {
                        return Some(n);
                    }
                    if let Some(n) = scan_stmts(else_branch, readonly) {
                        return Some(n);
                    }
                }
                Stmt::For { body, .. }
                | Stmt::While { body, .. }
                | Stmt::Loop { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Default(body)
                | Stmt::Catch(body)
                | Stmt::Control(body) => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                Stmt::Given { body, .. } | Stmt::When { body, .. } => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                _ => {}
            }
            None
        }
        if let Some(var_name) = scan_stmts(body, &readonly_native_params) {
            let msg = format!("Cannot assign to readonly variable {}", var_name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("variable".to_string(), Value::str(var_name));
            attrs.insert("message".to_string(), Value::str(msg));
            return Some(Value::make_instance(
                crate::symbol::Symbol::intern("X::Assignment::RO::Comp"),
                attrs,
            ));
        }
        None
    }
}
