use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Compiler {
    /// Pre-qualify a class/role declaration's name with the compiler's
    /// `current_package` when compiling inside a `unit module`/`unit class`/
    /// `unit role` body. Bare names (no `::`) are rewritten to
    /// `Pkg::Name`. Names that already contain `::` or are top-level
    /// (current_package == "GLOBAL") are returned unchanged.
    pub(super) fn qualify_decl_name(&self, stmt: &Stmt) -> Stmt {
        if !self.in_unit_package
            || self.current_package == "GLOBAL"
            || self.current_package.contains("::&")
        {
            return stmt.clone();
        }
        let bare = match stmt {
            Stmt::ClassDecl { name, .. } | Stmt::RoleDecl { name, .. } => name.resolve(),
            _ => return stmt.clone(),
        };
        if bare.contains("::") {
            return stmt.clone();
        }
        let qualified = format!("{}::{}", self.current_package, bare);
        let qualified_sym = Symbol::intern(&qualified);
        let pkg = self.current_package.clone();
        let qualify_parent = |p: &String| -> String {
            if p.contains("::") || p.is_empty() {
                return p.clone();
            }
            // Well-known builtin types should not be package-qualified.
            // "Grammar" inside a module should stay "Grammar", not become
            // "MyModule::Grammar".
            if matches!(
                p.as_str(),
                "Any"
                    | "Cool"
                    | "Mu"
                    | "Grammar"
                    | "Match"
                    | "Int"
                    | "Str"
                    | "Num"
                    | "Rat"
                    | "Bool"
                    | "IO"
                    | "Exception"
                    | "Stash"
                    | "Array"
                    | "Hash"
                    | "List"
                    | "Map"
                    | "Set"
                    | "Bag"
                    | "Mix"
                    | "Range"
                    | "Pair"
                    | "Regex"
                    | "FatRat"
                    | "Complex"
                    | "Callable"
                    | "Numeric"
                    | "Real"
                    | "Stringy"
                    | "Positional"
                    | "Associative"
                    | "Proc"
                    | "Supply"
                    | "Supplier"
                    | "Date"
                    | "DateTime"
                    | "Capture"
                    | "Parameter"
                    | "Signature"
            ) {
                return p.clone();
            }
            format!("{}::{}", pkg, p)
        };
        match stmt {
            Stmt::ClassDecl {
                name_expr,
                parents,
                class_is_rw,
                is_hidden,
                is_lexical,
                hidden_parents,
                does_parents,
                repr,
                body,
                language_version,
                custom_traits,
                is_unit,
                decl_id,
                parent_args,
                ..
            } => {
                let new_parents: Vec<String> = parents.iter().map(&qualify_parent).collect();
                let new_does: Vec<String> = does_parents.iter().map(&qualify_parent).collect();
                let new_hidden: Vec<String> = hidden_parents.iter().map(&qualify_parent).collect();
                // Re-key alongside `parents`/`does_parents`/`hidden_parents`
                // above so `parent_args` lookups by the (now qualified)
                // parent string still hit (ADR-0019 D4-1).
                let new_parent_args: Vec<(String, Vec<Expr>)> = parent_args
                    .iter()
                    .map(|(k, v)| (qualify_parent(k), v.clone()))
                    .collect();
                Stmt::ClassDecl {
                    name: qualified_sym,
                    name_expr: name_expr.clone(),
                    parents: new_parents,
                    class_is_rw: *class_is_rw,
                    is_hidden: *is_hidden,
                    is_lexical: *is_lexical,
                    hidden_parents: new_hidden,
                    does_parents: new_does,
                    repr: repr.clone(),
                    body: body.clone(),
                    language_version: language_version.clone(),
                    custom_traits: custom_traits.clone(),
                    is_unit: *is_unit,
                    decl_id: *decl_id,
                    parent_args: new_parent_args,
                }
            }
            Stmt::RoleDecl {
                type_params,
                type_param_defs,
                is_export,
                export_tags,
                body,
                is_rw,
                language_version,
                custom_traits,
                ..
            } => Stmt::RoleDecl {
                name: qualified_sym,
                type_params: type_params.clone(),
                type_param_defs: type_param_defs.clone(),
                is_export: *is_export,
                export_tags: export_tags.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                language_version: language_version.clone(),
                custom_traits: custom_traits.clone(),
            },
            _ => stmt.clone(),
        }
    }

    fn regex_match_returns_multiple(expr: &Expr) -> bool {
        let Expr::Binary { op, right, .. } = expr else {
            return false;
        };
        if !matches!(op, TokenKind::SmartMatch | TokenKind::BangTilde) {
            return false;
        }
        let regex = match right.as_ref() {
            Expr::MatchRegex(v) | Expr::Literal(v) => v,
            _ => return false,
        };
        matches!(
            regex.view(),
            ValueView::RegexWithAdverbs(a)
                if a.global || a.overlap || a.exhaustive || a.repeat.is_some()
        )
    }

    /// Returns true if the expression contains a state variable declaration at
    /// its OWN block level. Used to decide whether `StateVarInitGuard` can
    /// safely skip evaluation of a state variable's RHS initializer, and
    /// whether an inline nested block needs a `ResetStateLocals`.
    ///
    /// The walk descends through operator/call/subscript shapes but stops at
    /// anything that introduces a block of its own (`Block`, `Lambda`,
    /// `AnonSub`, `Gather`, ...): a `state` in there belongs to *that* clone
    /// and is reset at its entry, so descending would only make this block emit
    /// a redundant reset.
    ///
    /// Descending at all matters because a `state` declaration is usually not
    /// the whole expression: `++state $n` parses as a `Unary` around the decl,
    /// so a shallow test missed it and an `if` branch holding one never emitted
    /// its reset — `sub f { if 1 { ++state $n } }` counted 1, 2, 3 across calls
    /// where raku restarts at 1 each time.
    pub(super) fn expr_has_state_decl(expr: &Expr) -> bool {
        let any = |es: &[Expr]| es.iter().any(Self::expr_has_state_decl);
        match expr {
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { is_state: true, .. } => true,
                Stmt::VarDecl { expr, .. } | Stmt::Expr(expr) => Self::expr_has_state_decl(expr),
                _ => false,
            },
            Expr::Grouped(e)
            | Expr::Unary { expr: e, .. }
            | Expr::PostfixOp { expr: e, .. }
            | Expr::AssignExpr { expr: e, .. }
            | Expr::Itemize(e)
            | Expr::DeitemizeForBind(e)
            | Expr::Eager(e)
            | Expr::PositionalPair(e)
            | Expr::ZenSlice(e) => Self::expr_has_state_decl(e),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_state_decl(left) || Self::expr_has_state_decl(right)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::expr_has_state_decl(cond)
                    || Self::expr_has_state_decl(then_expr)
                    || Self::expr_has_state_decl(else_expr)
            }
            Expr::Index { target, index, .. } => {
                Self::expr_has_state_decl(target) || Self::expr_has_state_decl(index)
            }
            Expr::IndexAssign {
                target,
                index,
                value,
                ..
            } => {
                Self::expr_has_state_decl(target)
                    || Self::expr_has_state_decl(index)
                    || Self::expr_has_state_decl(value)
            }
            Expr::MethodCall { target, args, .. } | Expr::CallOn { target, args } => {
                Self::expr_has_state_decl(target) || any(args)
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => any(args),
            Expr::ArrayLiteral(es)
            | Expr::BracketArray(es, _)
            | Expr::CaptureLiteral(es)
            | Expr::StringInterpolation(es) => any(es),
            _ => false,
        }
    }

    /// Check if a default value expression statically mismatches a type constraint.
    /// Returns `Some(value_repr)` if a mismatch is detected, `None` otherwise.
    fn check_default_type_mismatch(type_constraint: &str, expr: &Expr) -> Option<String> {
        // Split off an optional type smiley (`:D` / `:U` / `:_`).
        let (effective_constraint, smiley) = if let Some(b) = type_constraint.strip_suffix(":D") {
            (b, Some('D'))
        } else if let Some(b) = type_constraint.strip_suffix(":U") {
            (b, Some('U'))
        } else if let Some(b) = type_constraint.strip_suffix(":_") {
            (b, Some('_'))
        } else {
            (type_constraint, None)
        };
        // Only a recognized concrete built-in type can be rejected at compile
        // time. A subset / `where`-constrained type (`my $x is default(42) where
        // * == 42`, compiled to an anonymous `__mutsu_anon_subset_N`) or any
        // user-defined type narrows membership by a runtime predicate the compiler
        // cannot evaluate, so it must NOT be statically flagged as a mismatch —
        // the default may well satisfy it. S02-types/whatever.t "compile time
        // WhateverCode / Junction evaluation" exercises exactly this.
        const CHECKABLE_BUILTINS: &[&str] = &[
            "Int", "Num", "Rat", "Bool", "Str", "Numeric", "Real", "Cool", "Any", "Mu", "Stringy",
            "Complex", "Rational",
        ];
        if !CHECKABLE_BUILTINS.contains(&effective_constraint) {
            return None;
        }
        // A concrete (defined) literal default can never bind to a `:U`
        // (type-object-only) constraint, e.g. `my Int:U $y is default(0)`.
        let is_concrete_literal = matches!(
            expr,
            Expr::Literal(lit)
                if matches!(
                    lit.view(),
                    ValueView::Int(_) | ValueView::Num(_) | ValueView::Str(_) | ValueView::Bool(_) | ValueView::Rat(..)
                )
        );
        if smiley == Some('U') && is_concrete_literal {
            return Some(match expr {
                Expr::Literal(v) => v.to_string_value(),
                _ => "?".to_string(),
            });
        }
        let value_type = match expr {
            Expr::Literal(lit) => match lit.view() {
                ValueView::Str(s) => {
                    if effective_constraint != "Str"
                        && effective_constraint != "Cool"
                        && effective_constraint != "Any"
                    {
                        return Some(s.to_string());
                    }
                    return None;
                }
                ValueView::Int(_) => "Int",
                ValueView::Num(_) => "Num",
                ValueView::Bool(_) => "Bool",
                ValueView::Nil => {
                    // Nil is invalid for typed variables (Int, Str, etc.)
                    // but valid for untyped (Any, Mu) or explicitly Nil-accepting types
                    if effective_constraint != "Any"
                        && effective_constraint != "Mu"
                        && !effective_constraint.contains("Nil")
                    {
                        return Some("Nil".to_string());
                    }
                    return None;
                }
                _ => return None,
            },
            _ => return None, // non-literal, can't check statically
        };
        // Check type hierarchy: Int matches Numeric, Cool, Any, etc.
        let mro: &[&str] = match value_type {
            "Bool" => &["Bool", "Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Int" => &["Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Rat" => &["Rat", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Stringy", "Cool", "Any", "Mu"],
            _ => &[],
        };
        if mro.contains(&effective_constraint) {
            None
        } else {
            Some(match expr {
                Expr::Literal(v) => v.to_string_value(),
                _ => "?".to_string(),
            })
        }
    }

    /// Slice 2a/2b (`docs/scalar-array-sharing.md`): `$scalar = @arr` / `$scalar
    /// = %hash` (and the chained `$scalar = $other`) shares the source container
    /// by reference (raku semantics) rather than snapshotting it. The source
    /// variable name is known at compile time, so flag the upcoming
    /// `SetLocal`/`AssignExpr` with `MarkArrayShareSource` to promote both to a
    /// shared `ContainerRef` cell. For a scalar source the runtime only shares
    /// when it actually holds a container (a plain `$x = $y` stays a copy).
    /// `@`/`%`/`&` targets (copy/bind elsewhere) are skipped.
    fn try_emit_array_share(&mut self, name: &str, expr: &Expr) -> bool {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return false;
        }
        let source = match expr {
            Expr::ArrayVar(n) => format!("@{}", n),
            Expr::HashVar(n) => format!("%{}", n),
            // Chained share: `$r = $q` where `$q` may hold a container. The
            // runtime no-ops when `$q` is a plain scalar, so this stays a copy.
            Expr::Var(n) => n.clone(),
            _ => return false,
        };
        self.with_escape(true, |c| c.compile_expr(expr));
        let name_idx = self.code.add_constant(Value::str(source));
        self.code.emit(OpCode::MarkArrayShareSource(name_idx));
        true
    }

    fn compile_assignment_rhs_for_target(&mut self, name: &str, expr: &Expr) {
        if self.try_emit_array_share(name, expr) {
            return;
        }
        // The RHS value is stored into the target, so a closure literal here
        // escapes the creating frame (escape analysis): force a shared cell for
        // the captured-and-mutated locals it closes over.
        self.with_escape(true, |c| c.compile_expr(expr));
        if !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && Self::regex_match_returns_multiple(expr)
        {
            self.code.emit(OpCode::ScalarizeRegexMatchResult);
        }
        // When assigning a `$` scalar variable to an `@` target, itemize
        // the value so it is treated as a single item (not flattened).
        // Sigilless variables (BareWord) are not itemized. A scalar bound
        // (`:=`) to a Positional is NOT a container, so use the var-aware
        // opcode which skips itemization for bound scalars.
        // A parenthesized single scalar (`@a = ($x)`) reaches here as
        // `Grouped(Var)` — unwrap it since `($x)` itemizes exactly like `$x`
        // (parens alone don't flatten; see roast S02-types/assigning-refs.t).
        let unwrapped = match expr {
            Expr::Grouped(inner) => inner.as_ref(),
            other => other,
        };
        if name.starts_with('@')
            && let Expr::Var(var_name) = unwrapped
        {
            let name_idx = self.code.add_constant(Value::str(var_name.clone()));
            self.code.emit(OpCode::ItemizeVar(name_idx));
        }
    }

    fn compile_condition_expr(&mut self, cond: &Expr) {
        match cond {
            Expr::Literal(lit)
                if matches!(
                    lit.view(),
                    ValueView::Regex(_) | ValueView::RegexWithAdverbs(..)
                ) =>
            {
                self.compile_expr(&Expr::MatchRegex(match cond {
                    Expr::Literal(v) => v.clone(),
                    _ => unreachable!(),
                }));
            }
            other => self.compile_expr(other),
        }
    }

    fn extract_test_more_plan_arg(arg: &Option<Expr>) -> Option<&Expr> {
        let expr = arg.as_ref()?;
        if let Expr::Binary {
            left,
            op: TokenKind::FatArrow,
            right,
        } = expr
            && matches!(
                left.as_ref(),
                Expr::Literal(lit) if matches!(lit.view(), ValueView::Str(key) if key.as_str() == "tests")
            )
        {
            return Some(right.as_ref());
        }
        None
    }

    fn compile_test_more_use(&mut self, arg: &Option<Expr>) {
        // `Test::More` is provided by native Test functions.
        let test_name_idx = self.code.add_constant(Value::str_from("Test"));
        self.code.emit(OpCode::UseModule {
            name_idx: test_name_idx,
            tags_idx: None,
            arg_count: 0,
        });
        if let Some(plan_arg) = Self::extract_test_more_plan_arg(arg) {
            self.compile_expr(plan_arg);
            let plan_name_idx = self.code.add_constant(Value::str_from("plan"));
            self.code.emit(OpCode::ExecCall {
                name_idx: plan_name_idx,
                arity: 1,
                arg_sources_idx: None,
            });
        }
    }

    /// Does `expr` ultimately root at a variable, so an `Index` over it is an
    /// assignable lvalue (e.g. `%h<k>`, `@a[i]`, `%h<a><b>`)? Function-call and
    /// other non-lvalue roots are excluded so we never synthesize a writeback
    /// assignment into a temporary (which would error where Raku is silent).
    fn for_element_container_is_lvalue(expr: &Expr) -> bool {
        match expr {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::BareWord(_) => true,
            Expr::Index { target, .. } => Self::for_element_container_is_lvalue(target),
            _ => false,
        }
    }

    /// Rewrite `for <ELEM>.values { ... }`, where `<ELEM>` is a var-rooted
    /// `Index` lvalue (`%h<k>` / `@a[i]` / `%h<a><b>`), into:
    ///
    ///   my @tmp = <ELEM>;          # copy the element array into a temp
    ///   for @tmp.values { ... };   # reuse the array-source per-element writeback
    ///   <ELEM> = @tmp;             # write the temp back into the element
    ///
    /// Returns `None` for anything but this exact shape (plain `@a`/`%h` sources
    /// are already handled by `for_iterable_source_name`).
    fn desugar_for_element_source(&mut self, stmt: &Stmt) -> Option<Vec<Stmt>> {
        let Stmt::For { iterable, .. } = stmt else {
            return None;
        };
        let Expr::MethodCall {
            target, name, args, ..
        } = iterable
        else {
            return None;
        };
        if !args.is_empty() || name.resolve() != "values" {
            return None;
        }
        let Expr::Index {
            target: container,
            index,
            is_positional,
        } = target.as_ref()
        else {
            return None;
        };
        if !Self::for_element_container_is_lvalue(container) {
            return None;
        }

        let tmp = format!("__for_elem_src_{}", self.code.constants.len());
        let element = target.as_ref().clone();

        // Copy the element's `.values` (not the element itself): for a
        // Positional element the two are the same list, but for a value whose
        // `.values` has its own semantics — a Match's Capture view
        // (`$m.<array>.values` over a quantified group) — assigning the bare
        // element to `@tmp` would wrap it as a one-element array and iterate
        // the wrong thing.
        let decl = Stmt::VarDecl {
            name: format!("@{}", tmp),
            expr: Expr::MethodCall {
                target: Box::new(element.clone()),
                name: crate::symbol::Symbol::intern("values"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            },
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: vec![("__has_initializer".to_string(), None)],
            where_constraint: None,
        };

        // The rewritten for-loop iterates `@tmp.values`; cloning the original
        // For and swapping only its iterable preserves params/body/label/mode.
        let mut for_stmt = stmt.clone();
        if let Stmt::For {
            iterable: new_iterable,
            ..
        } = &mut for_stmt
        {
            *new_iterable = Expr::MethodCall {
                target: Box::new(Expr::ArrayVar(tmp.clone())),
                name: crate::symbol::Symbol::intern("values"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
        }

        // Write the temp back only when the element actually is an Array —
        // the one case where `@tmp = <ELEM>.values` copies and mutations
        // would otherwise be lost. An unconditional write turned a
        // non-container element (`$match.<array>`, a Match — note Match does
        // Positional, so that role is not a usable guard) into a one-element
        // Array as a silent side effect of merely looping over its values.
        let writeback = Stmt::If {
            cond: Expr::Binary {
                op: crate::token_kind::TokenKind::SmartMatch,
                left: Box::new(element),
                right: Box::new(Expr::BareWord("Array".to_string())),
            },
            then_branch: vec![Stmt::Expr(Expr::IndexAssign {
                target: container.clone(),
                index: index.clone(),
                value: Box::new(Expr::ArrayVar(tmp)),
                is_positional: *is_positional,
            })],
            else_branch: Vec::new(),
            binding_var: None,
            is_statement_modifier: false,
        };

        Some(vec![decl, for_stmt, writeback])
    }

    /// Whether an `Index` expression's index is a syntactic shape that
    /// unambiguously produces a *slice* (several elements) rather than a
    /// single element: a `Range` (`1..3`, `1..^3`, ...), a comma list
    /// (`1, 3`), or bare `Whatever` (`*`). Conservative in the other
    /// direction: a plain scalar expression is assumed to index a single
    /// element even though it could dynamically hold a `Range` (Raku itself
    /// only knows at runtime) — matching this exactly is not needed for
    /// [`desugar_for_scalar_element_source`]'s purpose.
    fn for_index_is_slice(index: &Expr) -> bool {
        match index {
            Expr::Binary { op, .. } => matches!(
                op,
                crate::token_kind::TokenKind::DotDot
                    | crate::token_kind::TokenKind::DotDotCaret
                    | crate::token_kind::TokenKind::CaretDotDot
                    | crate::token_kind::TokenKind::CaretDotDotCaret
                    | crate::token_kind::TokenKind::DotDotDot
                    | crate::token_kind::TokenKind::DotDotDotCaret
            ),
            Expr::ArrayLiteral(_) | Expr::Whatever => true,
            _ => false,
        }
    }

    /// Rewrite `for <ELEM> { ... }`, where `<ELEM>` is a var-rooted `Index`
    /// lvalue (`%h<k>` / `@a[i]` / `%h<a><b>`) used *directly* as the loop
    /// source (no `.values`/similar wrapper — that shape is
    /// [`desugar_for_element_source`]), into:
    ///
    ///   my $tmp = <ELEM>;      # copy the element into a scalar temp
    ///   for $tmp { ... };      # reuse the scalar-topic per-iteration write-back
    ///   <ELEM> = $tmp;         # write the temp back into the element
    ///
    /// Raku topicalizes such an element as a single rw-aliased item (`for
    /// @a[i] { .=Int }` mutates `@a[i]`) — the same aliasing `given @a[i] {
    /// ... }` already gets via `TagElementSource`. `for` over a bare scalar
    /// variable already writes `$_`'s final value back to that variable, so
    /// routing through a temp variable needs no new VM machinery.
    fn desugar_for_scalar_element_source(&mut self, stmt: &Stmt) -> Option<Vec<Stmt>> {
        let Stmt::For { iterable, .. } = stmt else {
            return None;
        };
        let Expr::Index {
            target: container,
            index,
            is_positional,
        } = iterable
        else {
            return None;
        };
        if !Self::for_element_container_is_lvalue(container) {
            return None;
        }
        // A slice index (`@a[1..^3]`, `@a[1,3]`, `@a[*]`) yields *several*
        // elements, not one — rewriting through a scalar temp would collapse
        // the whole slice into a single topicalized value and only iterate
        // once (roast `S02-magicals/args.t`: `for @*ARGS[1..^+@*ARGS] { .say }`
        // must print each argument, not the slice as one item). Bail out for
        // every syntactic shape that is unambiguously a slice; anything else
        // (a plain scalar index expression) keeps the single-element rewrite.
        if Self::for_index_is_slice(index) {
            return None;
        }

        let tmp = format!("__for_scalar_elem_src_{}", self.code.constants.len());

        let decl = Stmt::VarDecl {
            name: tmp.clone(),
            expr: iterable.clone(),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: vec![("__has_initializer".to_string(), None)],
            where_constraint: None,
        };

        // The rewritten for-loop iterates the scalar temp; cloning the
        // original For and swapping only its iterable preserves
        // params/body/label/mode.
        let mut for_stmt = stmt.clone();
        if let Stmt::For {
            iterable: new_iterable,
            ..
        } = &mut for_stmt
        {
            *new_iterable = Expr::Var(tmp.clone());
        }

        let writeback = Stmt::Expr(Expr::IndexAssign {
            target: container.clone(),
            index: index.clone(),
            value: Box::new(Expr::Var(tmp)),
            is_positional: *is_positional,
        });

        Some(vec![decl, for_stmt, writeback])
    }

    /// Whether a bare statement expression yields a syntactically fresh rvalue
    /// (a method call / `Foo.new`) whose value may invoke a user-defined `sink`
    /// method in sink context. Bare variables (`$x;`) and function-call returns
    /// (`frob();`, possibly `is rw` → container) are excluded: Raku keeps those
    /// container-wrapped and does not auto-sink them, and mutsu decontainerizes
    /// before `SinkPop` so the two cases are indistinguishable at runtime.
    fn stmt_value_may_user_sink(expr: &Expr) -> bool {
        match expr {
            Expr::MethodCall { .. } => true,
            Expr::DoStmt(inner) => match inner.as_ref() {
                // `do { ... }` carries the value of its last statement.
                Stmt::Expr(e) => Self::stmt_value_may_user_sink(e),
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether a bare statement expression is a pure container read — a bare
    /// variable mention (`$f;`, `@a;`, `%h;`) — rather than a freshly computed
    /// value. Raku's optimizer recognizes a bare variable mention as "Useless
    /// use of ... in sink context" (`parser::sink_warn::describe_useless`) and
    /// never actually forces/sinks it, so a stored unhandled `Failure` must
    /// not explode merely because the bare mention was reached: Raku decides
    /// a Failure's fate at *construction* time (throwing immediately there
    /// under `use fatal` — matched by the various `self.fatal_mode`
    /// assignment-time checks in the VM), not by re-examining it at every
    /// later mention. `my $f = "a".Int; { use fatal; $f; }` must not throw —
    /// `$f` was made without fatal, so it stays soft forever; the same is
    /// true even with no `use fatal` anywhere at all (`my $f = "a".Int; $f;`
    /// lives too).
    ///
    /// Deliberately narrower than `sink_warn::describe_useless` (which also
    /// covers literals and pure operators): those can never evaluate to a
    /// *stored* Failure, so only the bare-variable shapes matter here. A
    /// sigil-prefixed `Var` name is a synthetic bind-desugaring artifact (see
    /// `describe_useless`'s identical exclusion), not a user-written bare
    /// scalar — treated conservatively as forcing.
    pub(super) fn stmt_value_is_bare_container_read(expr: &Expr) -> bool {
        match expr {
            Expr::Grouped(inner) => Self::stmt_value_is_bare_container_read(inner),
            Expr::Var(n) if n.starts_with(['$', '@', '%', '&']) => false,
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => true,
            _ => false,
        }
    }

    /// Whether a statement-expression's value can only be the value of an
    /// element assignment — directly (`%h{$k} = ...;`) or behind an
    /// `if`/`unless` statement modifier. Deliberately does NOT descend into a
    /// `with`/`given` modifier: rakudo's topicalizing modifiers sink the
    /// assignment's value and DO throw a stored unhandled Failure
    /// (`%h{$k} = .UInt with "-1";` explodes in rakudo; the `if` form and the
    /// bare form do not — verified against raku 2026-08-09).
    fn stmt_value_is_assignment(expr: &Expr) -> bool {
        fn tail_is_assignment(stmts: &[Stmt]) -> bool {
            stmts
                .iter()
                .rev()
                .find(|s| !matches!(s, Stmt::SetLine(_)))
                .is_some_and(|s| match s {
                    Stmt::Expr(e) => Compiler::stmt_value_is_assignment(e),
                    _ => false,
                })
        }
        match expr {
            Expr::IndexAssign { .. } | Expr::MultiDimIndexAssign { .. } => true,
            Expr::DoStmt(inner) => match inner.as_ref() {
                Stmt::Expr(e) => Self::stmt_value_is_assignment(e),
                Stmt::If {
                    then_branch,
                    else_branch,
                    is_statement_modifier: true,
                    ..
                } => else_branch.is_empty() && tail_is_assignment(then_branch),
                _ => false,
            },
            _ => false,
        }
    }

    /// Whether `expr` is directly a list-assignment call
    /// (`__mutsu_assign_callable_lvalue(ArrayLiteral([...]), [], rhs)`, the
    /// parser's lowering of `($a, $b) = ...`) to existing variables — the same
    /// shape `expr_call.rs`'s list-assign branch matches. Used only to decide
    /// whether the statement's own synthetic result value (needed for a
    /// chained/nested assignment, useless when the statement itself sinks its
    /// value) can be skipped; see `Compiler::sunk_list_assign_result`.
    fn is_list_assign_call(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Call { name, args }
                if name.resolve() == "__mutsu_assign_callable_lvalue"
                    && args.len() == 3
                    && matches!(&args[0], Expr::ArrayLiteral(targets) if targets.iter().all(|t| {
                        matches!(
                            t,
                            Expr::Var(_)
                                | Expr::ArrayVar(_)
                                | Expr::HashVar(_)
                                | Expr::Whatever
                                | Expr::Index { .. }
                                | Expr::MultiDimIndex { .. }
                        ) || matches!(t, Expr::DoStmt(s) if matches!(s.as_ref(), Stmt::VarDecl { .. }))
                    }))
        )
    }

    pub(super) fn compile_stmt(&mut self, stmt: &Stmt) {
        self.note_construct_body_block(stmt);
        match stmt {
            Stmt::Expr(expr) => {
                // See `Compiler::sunk_list_assign_result` — the statement's
                // value is about to be unconditionally discarded below, so a
                // top-level list assignment doesn't need to build its own
                // aliased result list.
                if Self::is_list_assign_call(expr) {
                    self.sunk_list_assign_result = true;
                }
                self.compile_condition_expr(expr);
                self.sunk_list_assign_result = false;
                // Assignment statements are wanted, not sunk (rakudo): storing
                // an unhandled Failure via `%h{$k} = ...;` / `@a[$i] = ...;`
                // (also with an `if`/`with` statement modifier) must not throw
                // it — except under `use fatal`, which the opcode handles.
                // Scalar `$x = ...` takes the Stmt::Assign path and already
                // behaves this way.
                if Self::stmt_value_is_assignment(expr) {
                    self.code.emit(OpCode::SinkPopAssign);
                } else {
                    self.code.emit(OpCode::SinkPop(
                        Self::stmt_value_may_user_sink(expr),
                        !Self::stmt_value_is_bare_container_read(expr),
                    ));
                }
            }
            // Feed split for an assignment to an already-declared variable:
            // `@x = SOURCE ==> SINK` — the feed operator is at Sequencer precedence
            // (looser than `=`), so it parses as `(@x = SOURCE) ==> SINK`. Relocate
            // the assignment into the feed's textually-left operand and run the feed
            // as a (sink-context) expression statement. (The `my`-declaration form
            // is split in the parser; see decl/my_decl_assign.rs.)
            Stmt::Assign {
                name,
                expr: feed @ Expr::Feed { .. },
                op: AssignOp::Assign,
            } if name != "*PID" => {
                let mut feed = feed.clone();
                {
                    let slot = crate::parser::feed_leftmost_operand_mut(&mut feed);
                    let source = std::mem::replace(slot, Expr::Literal(Value::NIL));
                    *slot = Expr::AssignExpr {
                        name: name.clone(),
                        expr: Box::new(source),
                        is_bind: false,
                    };
                }
                self.compile_condition_expr(&feed);
                self.code.emit(OpCode::SinkPop(
                    Self::stmt_value_may_user_sink(&feed),
                    !Self::stmt_value_is_bare_container_read(&feed),
                ));
            }
            Stmt::Block(stmts) => {
                // Check for placeholder conflicts in blocks. Use the *shallow*
                // collector: a placeholder belongs to its innermost enclosing
                // block, so placeholders nested inside an inner closure
                // (`{ my $a; { $^a } }`) must NOT be attributed to this block
                // and falsely flagged as redeclaring this block's `my $a`.
                let placeholders = crate::ast::collect_placeholders_shallow(stmts);
                if !placeholders.is_empty()
                    && let Some(err_val) =
                        self.check_placeholder_conflicts(&placeholders, stmts, None)
                {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // ADR-0048 D3/D6: a bare `{ ... }` STATEMENT is a Block raku
                // invokes with ZERO arguments, so a placeholder it declares is
                // that block's own unsatisfied parameter -- `{ $^c }` dies with
                // "Too few positionals passed; expected 1 argument but got 0".
                // (This replaces the ad-hoc "Implicit placeholder parameters are
                // not available in bare nested blocks" string the two tail-block
                // sites used to emit, and additionally covers the NON-tail form,
                // which previously leaked the placeholder onto the enclosing
                // routine's signature.)
                //
                // Two shapes are NOT such a block. A SYNTHESIZED body -- an
                // `if`/`while`/`loop` branch the compile sites re-wrap in
                // `Stmt::Block` -- is not a block of its own; `synthetic_block_body`
                // marks those, so peek it here rather than consuming it (it is
                // taken further down as `is_bare`). And a statement MODIFIER's
                // modified statement (`{ $a = $^x } unless 0`) IS this construct's
                // own block, supplied the modifier's value -- see
                // `is_construct_body_block`.
                if !self.synthetic_block_body
                    && !self.is_construct_body_block(stmts)
                    && self.emit_inlined_body_placeholder_binds(stmts, ArgSupply::None)
                {
                    return;
                }
                // A block with a top-level `when`/`default` is where that
                // `when`'s succeed stops unwinding: `given 5 { { when Int { } };
                // say "after" }` still runs the `say` (see
                // `OpCode::SucceedBarrier`). Emitted after the placeholder bail-out
                // above so the barrier is never left unpatched.
                let succeed_barrier_idx = Self::body_has_toplevel_when(stmts)
                    .then(|| self.code.emit(OpCode::SucceedBarrier { body_end: 0 }));
                let saved_dynamic_scope = self.push_dynamic_scope_lexical();
                self.seed_user_listop_shadows(stmts);
                // Snapshot the sigilless bindings that name a native lowercase
                // type (`str`/`int`/...). A `my \str` declared *inside* this block
                // is lexically scoped to it, so it must stop shadowing the native
                // type once the block ends; drop any such name the block newly
                // registers on exit. Scoped to type names only to keep the
                // (pre-existing) leak behaviour of ordinary sigilless names.
                let sigilless_type_names_before: std::collections::HashSet<String> = self
                    .sigilless_locals
                    .iter()
                    .filter(|n| crate::runtime::Interpreter::is_builtin_type(n))
                    .cloned()
                    .collect();
                // A genuine source `{ ... }` is a Raku callframe (it contributes
                // an anonymous frame to a backtrace captured inside it); a
                // synthesized if/while/loop body is not. `synthetic_block_body`
                // is set by those compile sites; consume it here.
                let is_bare = !std::mem::take(&mut self.synthetic_block_body);
                // A genuine source block is re-cloned every time its enclosing
                // block runs, so its own `state` restarts per execution — see
                // `OpCode::ResetStateLocals`. A SYNTHETIC body is excluded: a
                // loop body is the block the loop statement clones ONCE (its
                // iterations share the state), and an `if` branch already got
                // its reset at the branch site. A sole-block loop body
                // (`{ ... } for @xs`) is likewise the loop's own body — the
                // loop compile sites set `suppress_loop_block_state_reset` so
                // its `state` persists across iterations.
                let suppress_loop_reset = std::mem::take(&mut self.suppress_loop_block_state_reset);
                let state_reset = (is_bare && !suppress_loop_reset)
                    .then(|| self.emit_nested_block_state_reset(stmts))
                    .flatten();
                if Self::has_catch_or_control(stmts) {
                    self.next_try_is_bare_block = is_bare;
                    self.compile_implicit_try(stmts);
                    self.next_try_is_bare_block = false;
                    self.code.emit(OpCode::Pop);
                } else if Self::has_block_enter_leave_phasers(stmts) {
                    self.compile_phaser_block_scope(stmts, PhaserBlockResult::Discard);
                } else if Self::has_let_deep(stmts) {
                    // Block contains `let`/`temp` — wrap in LetBlock for save/restore
                    let idx = self.code.emit(OpCode::LetBlock { body_end: 0 });
                    let needs_topic = Self::has_real_let_deep(stmts);
                    for (i, s) in stmts.iter().enumerate() {
                        let is_last = i == stmts.len() - 1;
                        if is_last && needs_topic {
                            // For `let` blocks, set topic from the last statement's
                            // value so we can check success/failure
                            self.compile_last_stmt_as_topic(s);
                        } else {
                            self.compile_stmt(s);
                        }
                    }
                    self.code.patch_let_block_end(idx);
                } else if Self::has_use_stmt(stmts) {
                    // Block contains `use` — wrap with import scope save/restore
                    // so imports are lexically scoped to this block
                    self.code.emit(OpCode::PushImportScope);
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                    self.code.emit(OpCode::PopImportScope);
                } else {
                    // Plain blocks still create a lexical routine scope.
                    // `BlockScope` snapshots env before the body and restores
                    // it after (dropping any new env key), so a `my TYPE $x`
                    // compiled while this flag is set can safely use the
                    // env-only SetVarTypeScoped opcode — see
                    // `lexically_in_block`'s doc comment.
                    let saved_lexically_in_block =
                        std::mem::replace(&mut self.lexically_in_block, true);
                    let idx = self.code.emit(OpCode::BlockScope {
                        pre_end: 0,
                        enter_end: 0,
                        body_end: 0,
                        keep_start: 0,
                        undo_start: 0,
                        post_start: 0,
                        end: 0,
                        is_bare_block: is_bare,
                    });
                    self.code.patch_block_pre_end(idx);
                    self.code.patch_block_enter_end(idx);
                    // Raku's `my` declarations are visible for the entire
                    // enclosing block, even though the value is only
                    // (re-)initialized when execution reaches the declaration
                    // statement. mutsu's per-routine local-slot storage
                    // (`alloc_local` reuses the slot for a same-named
                    // declaration) means a block's own `my $x` only shadows a
                    // same-named outer local once its VarDecl statement
                    // actually runs — so a hoisted nested `sub` invoked (via
                    // forward reference) before that point would wrongly
                    // observe the OUTER value instead of an undefined one.
                    // Reset such shadowing slots to Nil right at block entry,
                    // before hoisting nested subs, so the shadow is visible
                    // from the very start of the block (roast
                    // S04-declarations/my-6e.t: "declared below the calling
                    // position"). Scoped narrowly to blocks that actually
                    // declare a nested `sub` (the only way to observe the
                    // early value) and to plain `$`-sigil scalars, to avoid
                    // clobbering a lingering type constraint that a sibling
                    // block left on a reused `@`/`%` slot (e.g. `my Int @a`
                    // followed later by an untyped `my @a` sharing the slot —
                    // an unconditional Nil reset there would fail the typed
                    // slot's type check; a real initializer's value normally
                    // satisfies it, so only the premature reset is unsafe).
                    if stmts.iter().any(|s| matches!(s, Stmt::SubDecl { .. })) {
                        for s in stmts.iter() {
                            if let Stmt::VarDecl {
                                name,
                                is_state: false,
                                is_our: false,
                                custom_traits,
                                ..
                            } = s
                                // Plain lexical scalars store a bare, sigil-stripped
                                // name (`my $x` -> "x"); `@`/`%`/`&` sigils and
                                // twigils (`.`/`!`/`*`) keep their marker prefix.
                                && !name.starts_with('@')
                                && !name.starts_with('%')
                                && !name.starts_with('&')
                                && !name.starts_with('.')
                                && !name.starts_with('!')
                                && !name.starts_with('*')
                                && !name.contains("::")
                                && let Some(&slot) = self.local_map.get(name.as_str())
                                && !custom_traits.iter().any(|(t, _)| t == "__constant")
                            {
                                self.code.emit(OpCode::LoadNil);
                                self.code.emit(OpCode::SetLocal(slot));
                            }
                        }
                    }
                    // Hoist sub declarations: register subs first so forward
                    // references like `&fa` work before the sub is textually
                    // declared (Raku sub hoisting semantics).
                    // Strip non-internal custom traits during hoisting — types/roles
                    // may not be registered yet; traits are applied during the normal pass.
                    for s in stmts.iter() {
                        if let Stmt::SubDecl { .. } = s {
                            let mut hoisted = s.clone();
                            if let Stmt::SubDecl { custom_traits, .. } = &mut hoisted {
                                custom_traits.retain(|(t, _)| {
                                    t.starts_with("__")
                                        || t == "default"
                                        || t.starts_with("DEPRECATED")
                                });
                            }
                            self.compile_stmt(&hoisted);
                        }
                    }
                    for s in stmts {
                        self.compile_stmt(s);
                    }
                    self.code.patch_block_body_end(idx);
                    self.code.patch_block_keep_start(idx);
                    self.code.patch_block_undo_start(idx);
                    self.code.patch_block_post_start(idx);
                    self.code.patch_loop_end(idx);
                    self.lexically_in_block = saved_lexically_in_block;
                }
                self.sigilless_locals.retain(|n| {
                    sigilless_type_names_before.contains(n)
                        || !crate::runtime::Interpreter::is_builtin_type(n)
                });
                self.pop_dynamic_scope_lexical(saved_dynamic_scope);
                self.patch_nested_block_state_reset(state_reset);
                if let Some(idx) = succeed_barrier_idx {
                    self.code.patch_succeed_barrier_body_end(idx);
                }
            }
            Stmt::SyntheticBlock(stmts) => {
                // Detect `:=` bind context for `@` variables: the parser wraps
                // `my @a := expr` in a SyntheticBlock containing VarDecl followed
                // by `__mutsu_record_bound_array_len`.  Set bind_vardecl so the
                // VarDecl compilation emits MarkBindContext before SetLocal,
                // making the VM preserve the container type (e.g. List stays List).
                let has_bound_array_len = stmts.iter().any(|s| {
                    matches!(s,
                        Stmt::Expr(Expr::Call { name, .. })
                        if name.resolve() == "__mutsu_record_bound_array_len"
                    )
                });
                // Detect `:=` bind context for scalar variables via MarkBind.
                let has_mark_bind = stmts.iter().any(|s| matches!(s, Stmt::MarkBind));
                // Collect sigilless readonly names so we can clear the flag
                // before the VarDecl assignment (allows re-declaration in loops).
                let sigilless_readonly_names: Vec<String> = stmts
                    .iter()
                    .filter_map(|s| {
                        if let Stmt::MarkSigillessReadonly(name) = s {
                            Some(name.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                // Collect the bound array variable name for skipping the
                // trailing Var statement that would force the LazyList via SinkPop.
                let mut bound_array_var: Option<String> = None;
                for s in stmts {
                    if has_bound_array_len
                        && let Stmt::VarDecl { name, .. } = s
                        && name.starts_with('@')
                    {
                        self.bind_vardecl = true;
                        bound_array_var = Some(name.clone());
                    }
                    if has_mark_bind && matches!(s, Stmt::VarDecl { .. }) {
                        self.bind_vardecl = true;
                    }
                    // Before compiling a VarDecl that will be followed by
                    // MarkSigillessReadonly, clear the old readonly flag so
                    // that re-declaration in a loop iteration succeeds.
                    if let Stmt::VarDecl { name, .. } = s
                        && sigilless_readonly_names.contains(name)
                    {
                        // Register the sigilless name BEFORE compiling its
                        // VarDecl so the decl emits MarkConstantContext (a raw
                        // `\x = ...` binds the value itself — no Scalar
                        // container, so SetLocal must not itemize it).
                        self.sigilless_locals.insert(name.clone());
                        let key = format!("__mutsu_sigilless_readonly::{}", name);
                        let key_idx = self.code.add_constant(Value::str(key));
                        let false_idx = self.code.add_constant(Value::FALSE);
                        self.code.emit(OpCode::LoadConst(false_idx));
                        self.code.emit(OpCode::SetGlobal(key_idx));
                    }
                    // Skip the trailing Var(@name) expression for bound array
                    // declarations. Without this, SinkPop would eagerly force a
                    // lazy gather/take list bound via `:=`.
                    if let Some(ref bav) = bound_array_var
                        && let Stmt::Expr(Expr::Var(vname)) = s
                        && vname == bav
                    {
                        // Emit a Nil instead of the Var to avoid forcing.
                        self.code.emit(OpCode::LoadNil);
                        self.code.emit(OpCode::SinkPop(false, true));
                        continue;
                    }
                    self.compile_stmt(s);
                }
            }
            Stmt::MarkReadonly(name) => {
                let idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::MarkVarReadonly(idx));
            }
            Stmt::MarkBoundContainer(name) => {
                // Record `__mutsu_bound::NAME` = true in env so the whole-var
                // readonly check (`CheckReadOnly`) can tell a `:=`-bound
                // container (writable) apart from a `constant` one (immutable).
                let key = format!("__mutsu_bound::{}", name);
                let key_idx = self.code.add_constant(Value::str(key));
                let true_idx = self.code.add_constant(Value::TRUE);
                self.code.emit(OpCode::LoadConst(true_idx));
                self.code.emit(OpCode::SetGlobal(key_idx));
            }
            Stmt::MarkBind => {
                // Handled by SyntheticBlock detection; no-op when compiled standalone.
            }
            Stmt::MarkSigilless(name) => {
                // Track a sigilless local so BareWord compilation reads it from its
                // slot (GetLocal), not via GetBareWord/env. Unlike
                // MarkSigillessReadonly this does NOT set the readonly flag: a typed
                // sigilless bind (`my Int \d := 7`) keeps container mutability.
                self.sigilless_locals.insert(name.clone());
            }
            Stmt::MarkSigillessReadonly(name) => {
                // Track sigilless locals so BareWord compilation can
                // distinguish them from `$`-sigiled variables.
                self.sigilless_locals.insert(name.clone());
                // Set __mutsu_sigilless_readonly::NAME = true in env
                let key = format!("__mutsu_sigilless_readonly::{}", name);
                let key_idx = self.code.add_constant(Value::str(key));
                let true_idx = self.code.add_constant(Value::TRUE);
                self.code.emit(OpCode::LoadConst(true_idx));
                self.code.emit(OpCode::SetGlobal(key_idx));
            }
            Stmt::Say(exprs) => {
                self.compile_slurpy_out_args(exprs);
                self.code.emit(OpCode::Say(exprs.len() as u32));
            }
            Stmt::Put(exprs) => {
                self.compile_slurpy_out_args(exprs);
                self.code.emit(OpCode::Put(exprs.len() as u32));
            }
            Stmt::Print(exprs) => {
                self.compile_slurpy_out_args(exprs);
                self.code.emit(OpCode::Print(exprs.len() as u32));
            }
            Stmt::Note(exprs) => {
                self.compile_slurpy_out_args(exprs);
                self.code.emit(OpCode::Note(exprs.len() as u32));
            }
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                where_constraint,
            } => {
                // Snapshot-and-clear `bind_vardecl` immediately: it is a
                // one-shot signal meant for THIS declaration's own store
                // (set by an enclosing `SyntheticBlock`/inline-block for a
                // `:=` bind). Left set on `self` while the RHS below is
                // compiled, it would leak into any nested `my`-declared
                // variable inside that RHS (e.g. `@state` in `my @x := do {
                // my uint8 @state = 0..255; ...; @state }`), wrongly giving
                // that unrelated declaration bind-context treatment and
                // skipping the array-from-Range materialization it needs
                // ("Cannot modify an immutable Range").
                let bind_vardecl = self.bind_vardecl;
                self.bind_vardecl = false;
                // `my &infix:<+> = ...` installs a user operator just like a
                // `sub infix:<+>` does — disable constant folding (ADR-0006 §2.1).
                self.note_operator_decl(name);
                // Record this declaration for an enclosing scope-isolating
                // do-block (string-interpolation `{...}`) so it can revert
                // exactly its own block-local declarations on exit.
                self.record_block_decl(name);
                // Record the block's own `my` bindings so the closure-exit
                // caller-writeback scan can tell them apart from mutated
                // captured outers (see CompiledCode::my_declared_sym).
                // `state`/`our`/dynamic declarations intentionally outlive or
                // cross the frame and are excluded.
                if !*is_state && !*is_our && !*is_dynamic && !name.starts_with('*') {
                    self.code.my_declared_sym.insert(Symbol::intern(name));
                } else if !*is_state && !*is_our && name.starts_with('*') {
                    // A `my $*x` REdeclaration is block/invocation-scoped just
                    // like a plain `my`, but lives in its own set — see
                    // `CompiledCode::dynamic_declared_sym`. Consumed by the
                    // map/grep inline-loop save/restore and the closure-exit
                    // caller-writeback scan so the fresh binding never leaks
                    // out of the frame that made it, while a plain `$*x = ...`
                    // write-through (not a declaration) still propagates.
                    // BOTH env spellings are recorded: every dynamic write
                    // maintains a `$*x` twin key alongside `*x`
                    // (`set_env_with_main_alias_sym`'s `twigil_dynamic_alias`
                    // mirror), and a read of `*x` falls back to `$*x`, so
                    // restoring/skipping only the bare key would leave the
                    // declaration readable through the twin.
                    self.code.dynamic_declared_sym.insert(Symbol::intern(name));
                    self.code
                        .dynamic_declared_sym
                        .insert(Symbol::intern(&format!("${}", name)));
                }
                // An inline `where` constraint on a scalar/sigilless variable
                // (e.g. `my $x where * > 0`, `my Int $n where { $_ %% 2 }`,
                // `my $v where &predicate`) is desugared into an anonymous subset
                // whose base is the declared type (or `Any`) and whose predicate is
                // the `where` expression. Reusing the subset machinery means the
                // existing type-constraint enforcement (TypeCheck on init and on
                // every assignment) checks the predicate for free. Collection
                // variables (`@`/`%`) are left untouched: a `where` there applies
                // to the whole container, which we do not yet model this way.
                let owned_type_constraint: Option<String> = match where_constraint {
                    Some(wc) if !name.starts_with('@') && !name.starts_with('%') => {
                        let anon = format!("__mutsu_anon_subset_{}", self.tmp_counter);
                        self.tmp_counter += 1;
                        let base = type_constraint.clone().unwrap_or_else(|| "Any".to_string());
                        let subset_stmt = Stmt::SubsetDecl {
                            name: Symbol::intern(&anon),
                            base,
                            predicate: Some((**wc).clone()),
                            version: String::new(),
                            is_export: false,
                            export_tags: Vec::new(),
                            // Anonymous where-subsets are internal; never
                            // alias them under the enclosing package.
                            is_my: true,
                        };
                        let idx = self.code.add_stmt(subset_stmt);
                        self.code.emit(OpCode::RegisterSubset(idx));
                        Some(anon)
                    }
                    _ => type_constraint.clone(),
                };
                // A `::T` type-capture constraint (`my ::a $a`) introduces a
                // fresh type variable rather than naming an existing type. With
                // no value to capture from, the captured type is unconstrained
                // (Mu), so we drop the constraint instead of trying to resolve
                // `::a` as a real type (which would die "Type '::a' is not
                // declared").
                // TODO: bind the capture name (`a`) as a type alias for the
                // declared/inferred type so `::a` is usable as a type later.
                let owned_type_constraint = match owned_type_constraint {
                    Some(tc) if tc.starts_with("::") => None,
                    other => other,
                };
                let type_constraint = &owned_type_constraint;
                // X::Dynamic::Package / X::Dynamic::Postdeclaration checks (see
                // `check_dynamic_var_decl_errors` doc comment).
                if self.check_dynamic_var_decl_errors(name) {
                    return;
                }
                // Track constant declarations so the compiler can avoid itemizing
                // them in `for` loops (constants have no Scalar container).
                let is_constant_decl = custom_traits.iter().any(|(t, _)| t == "__constant");
                // A `constant` that shadows an outer constant of the same name
                // (in an enclosing block or closure) is a fresh lexical binding,
                // not a reassignment of the outer package symbol. It must read
                // from its own local slot only and must NOT clobber the outer
                // constant's shared package store (`SetGlobalRaw`). Detect the
                // shadow *before* inserting this decl into the in-scope set — a
                // same-scope duplicate errors as X::Redeclaration below, so a hit
                // here is always an outer shadow.
                let shadows_outer_constant = is_constant_decl
                    && (self.constant_vars_in_scope.contains(name.as_str())
                        || self.outer_constant_names.contains(name.as_str()));
                if is_constant_decl {
                    // X::Redeclaration on a duplicate same-scope `constant` is only
                    // fired when the *sigil* matches. mutsu's AST strips the `$`
                    // from a scalar constant name, so `constant sym` (sigilless)
                    // and `constant $sym` (scalar) both arrive here as "sym"; firing
                    // on the bare name alone would wrongly reject that legal pair
                    // (see roast S06-operator-overloading/sub.t). Key the
                    // duplicate-detection set by the source sigil so only true
                    // same-sigil redeclarations are caught.
                    let constant_sigil = custom_traits
                        .iter()
                        .find(|(t, _)| t == "__constant_sigil")
                        .and_then(|(_, e)| match e {
                            Some(Expr::Literal(lit)) => match lit.view() {
                                ValueView::Str(s) => Some(s.to_string()),
                                _ => None,
                            },
                            _ => None,
                        })
                        .unwrap_or_default();
                    let redecl_key = format!("{}{}", constant_sigil, name);
                    if !self.constant_vars_current_scope.insert(redecl_key) {
                        let sym = name.trim_start_matches(['$', '@', '%', '&']).to_string();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(sym));
                        attrs.insert("what".to_string(), Value::str_from("symbol"));
                        let err = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                    self.constant_vars.insert(name.clone());
                    self.constant_vars_in_scope.insert(name.clone());
                    // A `constant` with a compile-time-constant scalar value is
                    // inlined at its read sites (ADR-0006 §2.2).
                    self.note_constant_decl(name, expr);
                } else {
                    // An ordinary `my`/`state` of the same bare name shadows the
                    // constant — mutsu strips sigils, so `my $DEBUG` and a
                    // sigilless `constant DEBUG` collide. Stop inlining it.
                    self.forget_constant(name);
                }
                // X::ParametricConstant: typed @/% constants are forbidden
                if is_constant_decl
                    && type_constraint.is_some()
                    && (name.starts_with('@') || name.starts_with('%'))
                {
                    let err = Value::make_instance(
                        Symbol::intern("X::ParametricConstant"),
                        std::collections::HashMap::new(),
                    );
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // Raku: redeclaring an existing same-scope `my` variable WITHOUT
                // an explicit initializer (`my $f` / `my Int $f`) is a no-op — the
                // variable keeps its current value (only a "Redeclaration of symbol"
                // warning would be emitted). A redeclaration WITH an initializer
                // (`my $f = 10`) still runs the assignment. `state`/`our`/`constant`
                // and `:=` binds have their own semantics and are excluded; a
                // `where`-constrained decl is desugared into a subset above and must
                // run. Tracking per-scope `my` names lets the bare-redeclaration
                // case suppress the reset that would otherwise clobber the value.
                let is_plain_my = !*is_state
                    && !*is_our
                    && !is_constant_decl
                    && !bind_vardecl
                    && where_constraint.is_none();
                if is_plain_my {
                    let has_init = custom_traits.iter().any(|(n, _)| n == "__has_initializer");
                    // Only the default-init form (a bare `my $f;` / `my Int $f;` /
                    // `my @a;` / `my %h;`, whose RHS is the synthesized empty type
                    // default) is a value-preserving no-op. A decl with a real RHS
                    // expression — including internal desugared temps like
                    // `@__destructure_tmp__` (an `Expr::ArrayLiteral`) that
                    // legitimately re-run on each `my (...)` destructure — must always
                    // execute. The synthesized defaults are, by sigil: `Literal(Nil)`
                    // ($ / &), an empty `Literal(Array)` (@), and an empty
                    // `Expr::Hash` (%).
                    let is_default_init = !has_init
                        && match expr {
                            Expr::Literal(lit) => match lit.view() {
                                ValueView::Nil => true,
                                ValueView::Array(ad, _) => ad.items().is_empty(),
                                _ => false,
                            },
                            Expr::Hash(pairs) => pairs.is_empty(),
                            _ => false,
                        };
                    let already_declared = !self.my_vars_current_scope.insert(name.clone());
                    if already_declared && is_default_init {
                        return;
                    }
                }
                let is_dynamic = *is_dynamic || self.var_is_dynamic(name);
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::SetVarDynamic {
                    name_idx,
                    dynamic: is_dynamic,
                });
                let has_default_trait = custom_traits.iter().any(|(n, _)| n == "default");
                let has_explicit_initializer =
                    custom_traits.iter().any(|(n, _)| n == "__has_initializer");
                let default_trait_expr =
                    custom_traits.iter().find_map(|(trait_name, trait_arg)| {
                        if trait_name == "default" {
                            trait_arg.as_ref()
                        } else {
                            None
                        }
                    });
                // Register type constraint early (for assignment checking) unless
                // `is default` trait is present — in that case defer until after
                // the trait is applied so the default value can be set first.
                if !has_default_trait && let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    self.emit_set_var_type(name, name_idx, tc_idx, *is_our);
                }
                // Record type constraint for compile-time literal type checks
                if let Some(tc) = type_constraint {
                    self.local_types.insert(name.clone(), tc.clone());
                }
                // For state variables, emit a guard that skips the RHS evaluation
                // when the state is already initialized (avoiding side effects).
                // Skip the guard if the RHS contains nested state declarations
                // (e.g. `state $a = state $b = 42`) since the inner state var
                // needs its StateVarInit to run on every call.
                let state_guard_idx = if *is_state && !Self::expr_has_state_decl(expr) {
                    // Pre-compute the state key early so the guard can reference it.
                    // We use a placeholder IP that will be unique enough.
                    let placeholder_ip = self.code.ops.len();
                    let key = format!(
                        "__state_{}::{}@{}",
                        self.current_package, name, placeholder_ip
                    );
                    let key_sym = Symbol::intern(&key);
                    let guard_idx = self.code.emit(OpCode::StateVarInitGuard(key_sym.id(), 0));
                    Some((guard_idx, key_sym))
                } else {
                    None
                };
                // For `our` redeclarations with no initializer (expr is Nil),
                // load the existing package variable value instead of
                // resetting to Nil. This makes `our $x = 3; ... our $x`
                // preserve the value 3 in the redeclaration.
                let is_our_redecl_nil =
                    *is_our && matches!(expr, Expr::Literal(lit) if lit.is_nil());
                // A scalar `:=` bind to a Positional makes the scalar a
                // non-container alias; record it so SetLocal can mark it
                // decontainerized (so `@a = $bound` flattens, not itemizes).
                // The parser tags such binds with the internal `__scalar_bind`
                // trait.
                let scalar_bind_decont = custom_traits.iter().any(|(t, _)| t == "__scalar_bind");
                // Capture whether this is a `:=` bind of an `@`/`%` container var
                // *before* the RHS-compilation branches below consume `bind_vardecl`.
                // Used by the `our` global store to skip the readonly check (the
                // var was marked readonly purely as a bind signal).
                let is_bound_container_vardecl =
                    bind_vardecl && (name.starts_with('@') || name.starts_with('%'));
                // A scalar `:=` bind (`my $x := EXPR`). Captured before the RHS
                // branches consume `bind_vardecl`; recorded on the CompiledCode so
                // `compute_free_vars` can vouch for it despite it reaching a call as
                // an argument (an immutable binding never goes stale). Both the
                // MarkBind form (`my $x := $y`, sets `bind_vardecl`) and the inline
                // `__scalar_bind` trait form (`my $x := my $y`) count.
                let is_scalar_colon_bind = (bind_vardecl || scalar_bind_decont)
                    && !name.starts_with('@')
                    && !name.starts_with('%')
                    && !name.starts_with('&');
                // A `constant` initializer is evaluated at BEGIN (compile) time,
                // so an uncaught exception while evaluating it surfaces as
                // X::Comp::BeginTime (with the original exception nested). Wrap
                // the RHS evaluation in a CheckPhaser scope so the top-level run
                // loop re-wraps any throw. Placed AFTER the X::Redeclaration /
                // X::ParametricConstant early-returns above so those compile-time
                // errors are not themselves wrapped.
                let constant_init_phaser_start = if is_constant_decl {
                    Some(self.code.emit(OpCode::CheckPhaserStart { end_ip: 0 }))
                } else {
                    None
                };
                if is_our_redecl_nil {
                    let qualified = self.qualify_our_variable_name(name);
                    let idx = self.code.add_constant(Value::str(qualified));
                    self.code.emit(OpCode::GetOurVar(idx));
                } else if bind_vardecl
                    && (!name.starts_with('@') && !name.starts_with('%')
                        || Self::is_simple_var_expr(expr)
                        // `my @slice := @array[1,2]` (an `@`-sigil bind to an
                        // array index/slice expression) must promote each
                        // indexed element to a shared `ContainerRef` cell —
                        // same autovivify-lazy + terminal machinery as a
                        // scalar element bind (`$x := @a[1]`) — so that a
                        // later whole-array assignment (`@slice = ...`)
                        // writes through the source array at a FIXED arity
                        // instead of snapshotting a disconnected copy.
                        || (name.starts_with('@')
                            && matches!(expr, Expr::Index { is_positional: true, .. })))
                {
                    // `:=` binding for VarDecl: use compile_call_arg so WrapVarRef
                    // is emitted and the VM can set up aliases.  For @/% targets,
                    // only emit WrapVarRef when the RHS is a simple variable.
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.bind_target_direct = true;
                    self.compile_call_arg(expr);
                    self.scalar_bind_autovivify = false;
                    self.bind_terminal = false;
                } else if scalar_bind_decont
                    && (matches!(expr, Expr::ArrayVar(_) | Expr::HashVar(_))
                        || matches!(expr, Expr::DoStmt(s) if matches!(s.as_ref(), Stmt::VarDecl { .. })))
                {
                    // A scalar `:=` bind to a *whole* container variable
                    // (`my $ref := @a` / `my $ref := %h`) must alias the same
                    // container, not snapshot it (so `$ref.push` mutates `@a`).
                    // Likewise a bind to an inline declaration (`my $a := my $b`,
                    // which the parser leaves as a bare `VarDecl` with the
                    // `__scalar_bind` trait rather than wrapping it in the
                    // MarkBind SyntheticBlock that `my $a := $b` gets) must alias
                    // the freshly-declared variable's container.
                    // Route through compile_call_arg so WrapVarRef is emitted and
                    // SetLocal's bind path shares one cell. (Scalar binds normally
                    // take the assignment path below, which only Arc-shares — a
                    // COW push would then detach the alias.)
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.bind_target_direct = true;
                    self.compile_call_arg(expr);
                    self.scalar_bind_autovivify = false;
                    self.bind_terminal = false;
                } else {
                    // An uninitialized `&`-sigil variable (`my &foo;` / `my &;`)
                    // defaults to the `Callable` type object, not Any/Nil.
                    let callable_default = Expr::BareWord("Callable".to_string());
                    // An uninitialized untyped `$` scalar (`my $x;`) holds the
                    // Any type object, not Nil (PLAN 8.5 step 3) — see
                    // `uninit_untyped_scalar_defaults_to_any`.
                    let any_default = Self::any_type_object_expr();
                    let rhs_expr = if has_default_trait
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                        && matches!(expr, Expr::Literal(lit) if lit.is_nil())
                    {
                        default_trait_expr.unwrap_or(expr)
                    } else if name.starts_with('&')
                        && !has_explicit_initializer
                        && type_constraint.is_none()
                        && matches!(expr, Expr::Literal(lit) if lit.is_nil())
                    {
                        // `my Int &a` carries `Int` as a *return*-type constraint,
                        // not a value type; substituting a Callable default there
                        // would trip the value type-check. Only default to Callable
                        // for the unconstrained `my &a` / `my &`.
                        &callable_default
                    } else if Self::uninit_untyped_scalar_defaults_to_any(
                        name,
                        expr,
                        type_constraint.as_deref(),
                        custom_traits,
                    ) {
                        &any_default
                    } else {
                        expr
                    };
                    self.compile_assignment_rhs_for_target(name, rhs_expr);
                    // `my $ref := $obj.attr`: flag the trailing accessor
                    // dispatch to return the attribute slot's ContainerRef cell
                    // (see MarkAccessorRefContext). Done HERE, on the normal
                    // assignment-RHS route, rather than by re-routing MethodCall
                    // RHS through compile_call_arg — that route compiles closure
                    // args as NON-escaping (the #2746 guard), which would unbox
                    // the captured outer writes of e.g. `my $v := lazy { $x++ }`
                    // (statement prefixes like `lazy`/`do` parse as MethodCall).
                    if scalar_bind_decont && matches!(rhs_expr, Expr::MethodCall { .. }) {
                        self.mark_trailing_method_call_as_accessor_ref();
                    }
                }
                if let Some(start_idx) = constant_init_phaser_start {
                    self.code.emit(OpCode::CheckPhaserEnd);
                    let end_ip = self.code.ops.len() as u32;
                    if let OpCode::CheckPhaserStart { end_ip: ref mut e } = self.code.ops[start_idx]
                    {
                        *e = end_ip;
                    }
                }
                // `constant @x = ...` should store a List, not an Array.
                // Coerce the value on the stack before storing.
                if name.starts_with('@') && custom_traits.iter().any(|(t, _)| t == "__constant") {
                    self.code.emit(OpCode::CoerceToList);
                }
                // Skip TypeCheck for hash declarations: the type constraint
                // applies to element values, not to the collection itself.
                // TODO: enforce per-element type constraints at assignment time.
                let is_hash = name.starts_with('%');
                let is_native_type = type_constraint.as_ref().is_some_and(|tc| {
                    crate::runtime::native_types::is_native_int_type(tc)
                        || matches!(tc.as_str(), "num" | "num32" | "num64" | "str")
                });
                if let Some(tc) = type_constraint
                    && !is_hash
                    && !has_default_trait
                    && !(has_explicit_initializer
                        && matches!(expr, Expr::Literal(lit) if lit.is_nil())
                        && !is_native_type)
                {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    // Build the display name for error messages (e.g. "a" -> "$a")
                    let display_name = if name.starts_with('@')
                        || name.starts_with('%')
                        || name.starts_with('&')
                    {
                        name.clone()
                    } else {
                        format!("${}", name)
                    };
                    let var_name_idx = self.code.add_constant(Value::str(display_name));
                    // A `:=` bind to a typed scalar reports X::TypeCheck::Binding
                    // on mismatch (e.g. `my Str $x := 3`), not Assignment.
                    if scalar_bind_decont && !name.starts_with('@') && !name.starts_with('%') {
                        self.code
                            .emit(OpCode::TypeCheckBind(tc_idx, Some(var_name_idx)));
                    } else {
                        self.code
                            .emit(OpCode::TypeCheck(tc_idx, Some(var_name_idx)));
                    }
                }
                let slot = self.declare_local(name);
                if is_scalar_colon_bind {
                    let sym = Symbol::intern(name);
                    if !self.code.scalar_bind_locals.contains(&sym) {
                        self.code.scalar_bind_locals.push(sym);
                    }
                    // A `:=` bind installs the RHS without a Scalar container, so
                    // `for $x` iterates the bound value's elements. The one
                    // exception is a bind to a plain itemized scalar
                    // (`my $x := $itemized`), which inherits the item container;
                    // a bind to another already-non-itemized bound scalar stays
                    // non-itemized. Classify the RHS to distinguish these.
                    let rhs_is_itemized_scalar = match expr {
                        Expr::Var(rhs) => !self.noncontainer_bound_vars.contains(rhs),
                        Expr::Grouped(inner) => match inner.as_ref() {
                            Expr::Var(rhs) => !self.noncontainer_bound_vars.contains(rhs),
                            _ => false,
                        },
                        _ => false,
                    };
                    if !rhs_is_itemized_scalar {
                        self.noncontainer_bound_vars.insert(name.clone());
                    } else {
                        self.noncontainer_bound_vars.remove(name);
                    }
                }
                if *is_state {
                    if let Some((guard_idx, key_sym)) = state_guard_idx {
                        // Patch the guard jump target to the StateVarInit instruction
                        let state_init_ip = self.code.ops.len();
                        self.code.ops[guard_idx] =
                            OpCode::StateVarInitGuard(key_sym.id(), state_init_ip as u32);
                        self.code.state_locals.push((slot as usize, key_sym));
                        self.code.emit(OpCode::StateVarInit(slot, key_sym.id()));
                    } else {
                        // No guard (e.g., chained state declarations) — use the
                        // original approach where RHS is always evaluated.
                        let ip = self.code.ops.len();
                        let key = format!("__state_{}::{}@{}", self.current_package, name, ip);
                        let key_sym = Symbol::intern(&key);
                        self.code.state_locals.push((slot as usize, key_sym));
                        self.code.emit(OpCode::StateVarInit(slot, key_sym.id()));
                    }
                } else {
                    let is_constant = custom_traits.iter().any(|(t, _)| t == "__constant");
                    // A plain untyped scalar `our $x = <expr>` (no `:=` bind, no
                    // type constraint, no container sigil, no `constant`, no
                    // trait besides the internal "has an initializer" marker):
                    // install ONE shared `ContainerRef` cell under the lexical
                    // local slot AND the package-qualified name instead of the
                    // two-independent-stores sequence below. `our $x` and
                    // `$Pkg::x` (`$GLOBAL::x` at file scope) then name the SAME
                    // container — see `OpCode::DeclareOurScalar` and
                    // `docs/adr/README.md`-style rationale in
                    // news/2026-08/our-var-shared-cell.md. Every other `our`
                    // shape keeps the old two-store sequence below unchanged.
                    let use_our_cell = *is_our
                        && !shadows_outer_constant
                        && !is_constant
                        && !is_scalar_colon_bind
                        && !bind_vardecl
                        && type_constraint.is_none()
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                        && !name.starts_with('&')
                        && !self.sigilless_locals.contains(name.as_str())
                        && !has_default_trait
                        && !scalar_bind_decont
                        && custom_traits.iter().all(|(t, _)| t == "__has_initializer");
                    if use_our_cell {
                        let qualified = self.qualify_our_variable_name(name);
                        self.code
                            .our_locals
                            .push((slot as usize, qualified.clone()));
                        let qualified_idx = self.code.add_constant(Value::str(qualified));
                        self.code.emit(OpCode::DeclareOurScalar {
                            slot,
                            qualified_idx,
                        });
                    } else {
                        // For `our` we need a second copy of the value to store into the
                        // global. Normally we `Dup` the raw initializer up front, but for
                        // a constant the global store (`SetGlobalRaw`) coerces the value
                        // (e.g. calling `.Map` on a `%`-sigil RHS) — coercing the raw
                        // value a second time would invoke that side-effecting coercion
                        // twice. Instead, for constants we re-read the already-coerced
                        // value from the local slot via `GetLocal` after `SetLocal`.
                        if *is_our && !is_constant {
                            self.code.emit(OpCode::Dup);
                        }
                        if bind_vardecl && (name.starts_with('@') || name.starts_with('%')) {
                            self.code.emit(OpCode::MarkBindContext);
                        }
                        // Mark constant context so SetLocal uses List coercion for @ and
                        // skips Hash coercion for %, matching Raku's constant semantics.
                        // Scalar constants and sigilless declarations (`my \x = ...`)
                        // also carry the mark: both bind the value itself (no Scalar
                        // container), so SetLocal must not itemize it.
                        if is_constant || self.sigilless_locals.contains(name.as_str()) {
                            self.code.emit(OpCode::MarkConstantContext);
                        }
                        // A default-trait decl suppresses the explicit-initializer
                        // mark: its `is default(...)` is applied AFTER the store, so
                        // SetLocal's untyped-Nil-to-Any reset would fire before the
                        // default is registered and clobber a runtime-Nil value
                        // (`my $foo is default(Nil) = do without ... { $_ }` must
                        // keep Nil — S04-statements/with.t 49/56). The store keeps
                        // Nil verbatim and the ApplyVarTrait that follows replaces a
                        // still-Nil scalar with its default.
                        if has_explicit_initializer && !has_default_trait {
                            self.code.emit(OpCode::MarkExplicitInitializerContext);
                        }
                        // Mark this SetLocal as coming from a VarDecl so the VM
                        // can allow overwriting immutable containers (e.g. Blob)
                        // when the local slot is reused across loop iterations.
                        self.code.emit(OpCode::MarkVarDeclContext);
                        // A shaped declaration (`my @a[5] = ...`) keeps its declared
                        // shape; mark it so SetLocal does not strip the shape the way
                        // an unshaped value-copy (`my @u = @shaped`) does.
                        if custom_traits.iter().any(|(t, _)| t == "__shaped_decl") {
                            self.code.emit(OpCode::MarkShapedDeclContext);
                        }
                        // For % variables with QuantHash `is` traits, skip hash coercion
                        // so the trait handler gets the raw array/list value.
                        let has_quant_hash_trait = name.starts_with('%')
                            && custom_traits.iter().any(|(t, _)| {
                                let base = t.split('[').next().unwrap_or(t);
                                matches!(
                                    base,
                                    "BagHash" | "SetHash" | "MixHash" | "Bag" | "Set" | "Mix"
                                )
                            });
                        if has_quant_hash_trait {
                            self.code.emit(OpCode::MarkBindContext);
                        }
                        if scalar_bind_decont {
                            self.code.emit(OpCode::MarkScalarBindContext);
                        }
                        self.code.emit(OpCode::SetLocal(slot));
                        // A `constant` that shadows an outer constant of the same name
                        // (in an enclosing block or closure) is a fresh lexical binding,
                        // not a reassignment of the outer package symbol. Compile it as
                        // a pure `my` lexical: it lives only in its own local slot and
                        // never touches the shared package store (`our_locals` /
                        // `SetGlobalRaw`). Writing the package store would clobber the
                        // outer constant for sibling scopes, and the writeback merge for
                        // an `our` var declared inside a closure leaks the shadowing
                        // value back to the caller.
                        if *is_our && !shadows_outer_constant {
                            let qualified = self.qualify_our_variable_name(name);
                            // Track this slot as `our`-scoped so BlockScope restoration
                            // can sync the local from its global after block exit.
                            self.code
                                .our_locals
                                .push((slot as usize, qualified.clone()));
                            let idx = self.code.add_constant(Value::str(qualified));
                            // Constants should not have their values coerced by the
                            // @/% container rules: `constant @x` stores a List,
                            // `constant %x` stores a Map (not Array/Hash).
                            if is_constant {
                                // Re-read the value `SetLocal` already coerced (and
                                // cached in the slot) so `SetGlobalRaw` does not run
                                // the coercion — and its side effects — a second time.
                                self.code.emit(OpCode::GetLocal(slot));
                                self.code.emit(OpCode::SetGlobalRaw(idx));
                            } else {
                                // A `:=` bind of an `our` container var (`our %g := %h`)
                                // marks the var readonly as the bind signal; re-mark the
                                // bind context so the global store skips the readonly
                                // check (the mark is a bind signal, not a real RO).
                                if is_bound_container_vardecl {
                                    self.code.emit(OpCode::MarkBindContext);
                                }
                                self.code.emit(OpCode::SetGlobal(idx));
                            }
                        }
                    }
                }
                if *is_export {
                    let tags_idx = if export_tags.is_empty() {
                        None
                    } else {
                        let entries = export_tags
                            .iter()
                            .cloned()
                            .map(Value::str)
                            .collect::<Vec<Value>>();
                        Some(self.code.add_constant(Value::array(entries)))
                    };
                    self.code
                        .emit(OpCode::RegisterVarExport { name_idx, tags_idx });
                }
                for (trait_name, trait_arg) in custom_traits {
                    // Skip internal markers (not real traits)
                    if trait_name.starts_with("__") {
                        continue;
                    }
                    // `is default` on native types is not allowed
                    if trait_name == "default"
                        && let Some(tc) = type_constraint
                        && matches!(
                            tc.as_str(),
                            "int"
                                | "num"
                                | "str"
                                | "uint"
                                | "int8"
                                | "int16"
                                | "int32"
                                | "int64"
                                | "uint8"
                                | "uint16"
                                | "uint32"
                                | "uint64"
                                | "num32"
                                | "num64"
                        )
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(format!(
                            "X::Comp::Trait::NotOnNative: is default is not supported on native type {}",
                            tc
                        )));
                        attrs.insert("type".to_string(), Value::str("is".to_string()));
                        attrs.insert("subtype".to_string(), Value::str("default".to_string()));
                        let err = Value::make_instance(
                            Symbol::intern("X::Comp::Trait::NotOnNative"),
                            attrs,
                        );
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                    // `is default(expr)` with a type constraint: check that the
                    // default value is compatible with the type at compile time.
                    if trait_name == "default"
                        && let Some(tc) = type_constraint
                        && let Some(arg_expr) = trait_arg
                        && let Some(type_mismatch) = Self::check_default_type_mismatch(tc, arg_expr)
                    {
                        // Construct the expected type name based on sigil:
                        // @-sigil → Array[Type], %-sigil → Hash[Type], $-sigil → Type
                        let expected_type_name = if name.starts_with('@') {
                            format!("Array[{}]", tc)
                        } else if name.starts_with('%') {
                            format!("Hash[{}]", tc)
                        } else {
                            tc.to_string()
                        };
                        let err_msg = format!(
                            "X::Parameter::Default::TypeCheck: Default value '{}' will never bind to a parameter of type {}",
                            type_mismatch, expected_type_name
                        );
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("message".to_string(), Value::str(err_msg));
                        attrs.insert(
                            "expected".to_string(),
                            Value::package(Symbol::intern(&expected_type_name)),
                        );
                        attrs.insert(
                            "got".to_string(),
                            if type_mismatch == "Nil" {
                                Value::NIL
                            } else {
                                Value::str(type_mismatch.clone())
                            },
                        );
                        let err = Value::make_instance(
                            Symbol::intern("X::Parameter::Default::TypeCheck"),
                            attrs,
                        );
                        let idx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(idx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                    if let Some(arg) = trait_arg {
                        self.compile_expr(arg);
                    }
                    let trait_name_idx = self.code.add_constant(Value::str(trait_name.clone()));
                    self.code.emit(OpCode::ApplyVarTrait {
                        name_idx,
                        trait_name_idx,
                        has_arg: trait_arg.is_some(),
                        slot: self.local_map.get(name.as_str()).copied(),
                    });
                }
                // Deferred type constraint registration after traits are applied
                if has_default_trait && let Some(tc) = type_constraint {
                    let tc_idx = self.code.add_constant(Value::str(tc.clone()));
                    self.emit_set_var_type(name, name_idx, tc_idx, *is_our);
                }
                // Mark constant variables as readonly so that subsequent
                // assignments are rejected at runtime.
                if is_constant_decl {
                    self.code.emit(OpCode::MarkVarReadonly(name_idx));
                }
            }
            Stmt::Assign {
                name,
                expr,
                op: op @ (AssignOp::Assign | AssignOp::Bind),
            } if name != "*PID" => {
                // Handle $CALLER::varname = expr or $CALLER::varname := expr
                if let Some((bare_name, depth)) = Self::parse_caller_prefix(name) {
                    if matches!(op, AssignOp::Bind) {
                        // For := (bind), if the RHS is a variable, set up an alias
                        if let Expr::Var(rhs_name) = expr {
                            let target_idx = self.code.add_constant(Value::str(bare_name));
                            let source_idx = self.code.add_constant(Value::str(rhs_name.clone()));
                            self.code.emit(OpCode::BindCallerVar {
                                target_idx,
                                source_idx,
                                depth: depth as u32,
                            });
                        } else {
                            self.compile_expr(expr);
                            let name_idx = self.code.add_constant(Value::str(bare_name));
                            self.code.emit(OpCode::SetCallerVar {
                                name_idx,
                                depth: depth as u32,
                            });
                        }
                    } else {
                        self.compile_expr(expr);
                        let name_idx = self.code.add_constant(Value::str(bare_name));
                        self.code.emit(OpCode::SetCallerVar {
                            name_idx,
                            depth: depth as u32,
                        });
                    }
                    return;
                }
                // The invocant `self` is immutable — reject assignments at
                // compile time, but only inside a method body, where a bare
                // `self` names the invocant (roast S12-class/basic.t pins
                // `method f { self = 5 }` throwing). Outside a method the name
                // can only be an ordinary user variable (`my $self;
                // $self = $csv.header($fh)` — Text::CSV's 85_util.t), since
                // scalars are stored sigil-less and would otherwise collide.
                if name == "self" && self.lexically_in_method {
                    self.code.emit(OpCode::AssignReadOnly);
                    return;
                }
                if name.starts_with('&')
                    && !name.contains("::")
                    && !self.local_map.contains_key(name.as_str())
                    && !name.starts_with("&!")
                {
                    self.code.emit(OpCode::AssignReadOnly);
                    return;
                }
                // For &!attr (callable private attributes), strip the &
                // sigil so the env key matches the attribute name (!attr).
                let effective_name = if name.starts_with("&!") {
                    &name[1..]
                } else {
                    name.as_str()
                };
                // Compile-time check: assigning a numeric literal to a typed
                // numeric variable with a mismatched type (e.g. `my Num $n; $n = 42`)
                // should produce X::Syntax::Number::LiteralType.
                if matches!(op, AssignOp::Assign)
                    && let Some(err) = self.check_literal_type_mismatch(effective_name, expr)
                {
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // A genuine `$*x = ...` assignment to a never-declared dynamic var
                // throws X::Dynamic::NotFound. Only the plain Assign form (not `:=`)
                // reaches here; param binding / element auto-viv / `my` decls use
                // other paths and are intentionally exempt.
                if matches!(op, AssignOp::Assign) {
                    self.maybe_emit_dynamic_var_check(effective_name);
                }
                // Emit readonly check for assignment to potentially readonly params.
                // Skip the check for `:=` (rebinding replaces the container).
                let name_idx = self
                    .code
                    .add_constant(Value::str(effective_name.to_string()));
                // The anonymous state scalar (`$`, compiled as `__ANON_STATE__`)
                // can never be readonly via the sigilless-binding mechanism, and
                // SetGlobal performs its own readonly_vars check, so the extra
                // CheckReadOnly op (which allocates a `__mutsu_sigilless_readonly::`
                // lookup key on every assignment) is pure overhead here.
                if !matches!(op, AssignOp::Bind) && effective_name != "__ANON_STATE__" {
                    self.code.emit(OpCode::CheckReadOnly(name_idx));
                }
                if matches!(op, AssignOp::Bind) {
                    let mut scalar_elem_bind = false;
                    if effective_name.starts_with('@') || effective_name.starts_with('%') {
                        // A container rebind (`@a := ...`, `%h := ...`) replaces
                        // the whole container: mark the bind so SetGlobal rebinds
                        // the slot instead of assigning *into* the current value
                        // (which for an immutable QuantHash would wrongly throw
                        // "Cannot modify an immutable Set").
                        self.code.emit(OpCode::MarkBindContext);
                    } else if !effective_name.starts_with('&') {
                        // A scalar rebind (`$r := ...`) is still a bind: mark it
                        // so SetLocal records the bound-decont marker (it has no
                        // `__scalar_bind` trait like a `my $r := ...` VarDecl
                        // does). Without this the rebind is seen as a plain
                        // assignment and clears the marker, so `$r.VAR.^name`
                        // would wrongly report Scalar and `@a = $r` would itemize.
                        self.code.emit(OpCode::MarkScalarBindContext);
                        // A bound array/hash element (`$x := @a[1]`) must share a
                        // cell with the source slot so a later `$x = v` writes
                        // through to `@a[1]`, exactly like the `my $x := @a[1]`
                        // VarDecl path. Without cell promotion the rebind just
                        // snapshots the element value.
                        scalar_elem_bind = matches!(expr, Expr::Index { .. });
                    }
                    // Signal rebind context for cleanup of old bind pairs.
                    self.code.emit(OpCode::MarkRebindContext);
                    if scalar_elem_bind {
                        self.scalar_bind_autovivify = true;
                        self.bind_terminal = true;
                        self.bind_target_direct = true;
                        self.compile_call_arg(expr);
                        self.scalar_bind_autovivify = false;
                        self.bind_terminal = false;
                    } else {
                        self.compile_call_arg(expr);
                    }
                } else {
                    // Fuse `$x OP= rhs` (parsed as `$x = $x OP rhs`) into an
                    // atomic RMW for plain env-named scalars (Track C). The fused
                    // op leaves the result on the stack; statement context wants
                    // nothing, so discard it.
                    if self.try_compile_fused_compound_assign(effective_name, expr) {
                        self.code.emit(OpCode::Pop);
                        return;
                    }
                    self.compile_assignment_rhs_for_target(effective_name, expr);
                }
                // An assignment whose target is a sigilless binding (`-> \v`
                // loop-param bind stmts — `build_for_bind_stmts` strips the
                // `\` so the runtime cannot tell — or a write through a
                // sigilless alias) must NOT itemize the stored value: a
                // sigilless name is a non-container alias, so `\seed` bound
                // to a List stays a bare List (roast S03-sequence/exhaustive.t
                // drives `-> \description, \seed, ...` through these binds).
                if matches!(op, AssignOp::Assign) && self.sigilless_locals.contains(effective_name)
                {
                    self.code.emit(OpCode::MarkParamRawBindContext);
                }
                self.emit_set_named_var(effective_name);
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
                is_statement_modifier,
            } => {
                // Check for heredoc scope violations in then/else branches
                if let Some(err) = self.check_heredoc_scope_errors(then_branch) {
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                if let Some(err) = self.check_heredoc_scope_errors(else_branch) {
                    let idx = self.code.add_constant(err);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // Check if the then_branch uses @_ (bare if blocks receive
                // the condition value as @_ in Raku).
                let needs_at_underscore =
                    binding_var.is_none() && Self::body_uses_legacy_args(then_branch);
                // A bare `if EXPR { ... $^a ... }` whose block has a scalar
                // placeholder receives the condition value as that placeholder
                // (like `if EXPR -> $a { ... }`), so `if 42 { $^a.say }` prints 42.
                // The bind itself (and the arity failure when the branch declares
                // more placeholders than the single condition value satisfies) is
                // ADR-0048 D3's shared `emit_inlined_body_placeholder_binds`.
                //
                // An `if`/`unless`/`with`/`without` STATEMENT MODIFIER introduces
                // no block of its own (the oracle classifies it `Transparent`), so
                // its "body" placeholders are the enclosing routine's own
                // parameters: `sub f { say "$^a" if 1; 0 }; f(7)` must print 7, not
                // the condition. The two value-position `if` sites already carried
                // this guard; this one did not, which is why only the *non-tail*
                // statement-modifier form diverged.
                let bind_cond_placeholders = binding_var.is_none() && !*is_statement_modifier;
                let binds_cond_placeholder =
                    bind_cond_placeholders && Self::inlined_body_binds_supplied_value(then_branch);
                let needs_cond_value = needs_at_underscore || binds_cond_placeholder;
                // A condition that is a compile-time constant resolves the branch
                // here (ADR-0006 §2.2): `constant DEBUG = False; if DEBUG { note
                // ... }` emits nothing at all. The unreachable branch is only
                // dropped when it declares nothing (raku installs declarations
                // even in a never-taken branch) — otherwise the runtime branch is
                // compiled as usual.
                if binding_var.is_none()
                    && !needs_cond_value
                    && let Some(taken) = self.const_condition(cond)
                {
                    let (live, dead) = if taken {
                        (then_branch, else_branch)
                    } else {
                        (else_branch, then_branch)
                    };
                    if Self::branch_is_droppable(dead) {
                        self.compile_resolved_branch(live, *is_statement_modifier);
                        return;
                    }
                }
                // A pointy `if EXPR -> $_ { }` binds a FRESH lexical `$_` (like
                // `for -> $_`), so its topic must NOT flow back to an enclosing
                // `given $x`'s source variable. `EnterPointyTopic` saves + clears
                // `topic_source_var` for the branch; `ExitPointyTopic` (at the end)
                // restores it and the outer `$_`. Only the topic var `$_` needs it —
                // a named pointy (`-> $v`) declares its own lexical.
                let pointy_topic_scope = binding_var
                    .as_deref()
                    .is_some_and(|v| v.trim_start_matches('$') == "_");
                if pointy_topic_scope {
                    self.code.emit(OpCode::EnterPointyTopic);
                }
                let mut deferred_container_decl = None;
                if let Some(var_name) = binding_var {
                    // Desugar: if EXPR -> $var { BODY } else { ELSE }
                    // into: { my $var = EXPR; if $var { BODY } else { ELSE } }
                    let (desugared_cond, deferred) = self.compile_if_binding_decl(var_name, cond);
                    deferred_container_decl = deferred;
                    self.compile_condition_expr(&desugared_cond);
                } else {
                    self.compile_condition_expr(cond);
                    if needs_cond_value {
                        // Duplicate condition value: one for JumpIfFalse truthiness
                        // test, one for setting @_ / the placeholder in the then_branch.
                        self.code.emit(OpCode::Dup);
                    }
                }
                let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
                self.compile_if_binding_container_decl(&deferred_container_decl);
                if needs_at_underscore {
                    // Flatten the duplicated condition into @_ (like *@ slurpy).
                    self.code.emit(OpCode::FlattenSlurpy);
                    self.emit_set_named_var("@_");
                } else if bind_cond_placeholders {
                    // Bind the branch's placeholders to the (unflattened) condition
                    // value -- ADR-0048 D3. Emitted inside the taken branch so a
                    // never-taken `if 0 { "$^a $^b" }` raises nothing, matching raku.
                    self.emit_inlined_body_placeholder_binds(then_branch, ArgSupply::Condition);
                }
                // The branch is a block literal the enclosing block re-clones on
                // every execution, so its own `state` restarts each time — see
                // `OpCode::ResetStateLocals`.
                let then_state_reset =
                    self.emit_branch_state_reset(then_branch, *is_statement_modifier);
                if Self::has_block_enter_leave_phasers(then_branch) {
                    // A branch with ENTER/LEAVE/KEEP/UNDO phasers is a real
                    // block scope: its LEAVE must fire when the branch exits
                    // (OO::Monitors unlocks its monitor lock this way).
                    self.compile_phaser_block_scope(then_branch, PhaserBlockResult::Discard);
                } else if Self::body_mutates_topic(then_branch) {
                    self.synthetic_block_body = true;
                    self.compile_stmt(&Stmt::Block(then_branch.clone()));
                } else if Self::branch_declares_block_local(then_branch) {
                    self.compile_block_local_branch(then_branch);
                } else {
                    self.compile_body_with_implicit_try(then_branch);
                }
                self.patch_nested_block_state_reset(then_state_reset);
                if else_branch.is_empty() {
                    self.code.patch_jump(jump_else);
                    if needs_cond_value {
                        // Pop the leftover duplicated condition value on the
                        // false branch (JumpIfFalse consumed only one copy).
                        self.code.emit(OpCode::Pop);
                    }
                } else {
                    let jump_end = self.code.emit(OpCode::Jump(0));
                    self.code.patch_jump(jump_else);
                    if needs_cond_value {
                        self.code.emit(OpCode::Pop);
                    }
                    let else_state_reset =
                        self.emit_branch_state_reset(else_branch, *is_statement_modifier);
                    if else_branch.len() == 1 && matches!(else_branch[0], Stmt::If { .. }) {
                        self.compile_stmt(&else_branch[0]);
                    } else if Self::has_block_enter_leave_phasers(else_branch) {
                        self.compile_phaser_block_scope(else_branch, PhaserBlockResult::Discard);
                    } else if Self::body_mutates_topic(else_branch) {
                        self.synthetic_block_body = true;
                        self.compile_stmt(&Stmt::Block(else_branch.clone()));
                    } else if Self::branch_declares_block_local(else_branch) {
                        self.compile_block_local_branch(else_branch);
                    } else {
                        self.compile_body_with_implicit_try(else_branch);
                    }
                    self.patch_nested_block_state_reset(else_state_reset);
                    self.code.patch_jump(jump_end);
                }
                if pointy_topic_scope {
                    self.code.emit(OpCode::ExitPointyTopic);
                }
            }
            Stmt::While { cond, body, label } => {
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // When the loop body contains `$_ := expr` (topic rebind via `:=`), wrap
                // the body in a BlockScope so the rebind is lexically scoped per iteration.
                // BlockScope naturally prevents `$_` from propagating out.
                // Note: plain `$_ =` does NOT trigger wrapping — that would break `with`-block
                // topic restoration which relies on `body_mutates_topic` for isolation.
                let body_rebinds_topic = Self::body_rebinds_topic(&loop_body);
                let loop_idx = self.code.emit(OpCode::WhileLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                    collect: false,
                    isolate_topic: false,
                });
                self.compile_condition_expr(cond);
                self.code.patch_while_cond_end(loop_idx);
                if body_rebinds_topic {
                    self.synthetic_block_body = true;
                    self.compile_stmt(&Stmt::Block(loop_body.clone()));
                } else {
                    // A sole `{ ... }` in a prefix `while`/`until` body is a
                    // NESTED bare block that re-clones per iteration, so its
                    // `state` restarts (raku: 1 1 1) — no ResetStateLocals
                    // suppression here. The `{...} while COND` modifier form
                    // never calls the block in raku at all, so there is no
                    // persisting-modifier case to preserve (unlike `for`,
                    // which gates on `is_statement_modifier`).
                    //
                    // For ADR-0048 D3/D6 that sole block is still treated as
                    // this loop's own body rather than a nested zero-argument
                    // one: `Stmt::While` carries no `is_statement_modifier`
                    // flag, so `{ $a = $^x } while COND` and the far rarer
                    // `while COND { { $^a } }` are indistinguishable here, and
                    // the conservative choice is to not raise. Supplying the
                    // raw condition to either is D4/Phase 4's job. Re-noted
                    // because `expand_loop_phasers` rebuilt the body list.
                    self.note_construct_body_block_stmts(&loop_body);
                    self.compile_scope_restored_loop_body(&loop_body);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                params_def,
                body,
                label,
                mode,
                rw_block,
                explicit_zero_params,
                is_statement_modifier,
                uses_block_magic,
            } => {
                // `for @a[*] { ... }` — a whole-array Whatever slice iterates the
                // same elements as `for @a`, including aliasing for write-back
                // (`$_ = 9 for @a[*]` mutates @a). Normalize it to the plain array
                // source so it reuses the per-element write-back path. Restricted to
                // a var-rooted array target with the exact `[*]` index.
                if let Expr::Index {
                    target,
                    index,
                    is_positional: true,
                } = iterable
                    && matches!(index.as_ref(), Expr::Whatever)
                    && Self::for_single_array_source(target).is_some()
                {
                    let mut rewritten = stmt.clone();
                    if let Stmt::For { iterable: it, .. } = &mut rewritten {
                        *it = (**target).clone();
                    }
                    self.compile_stmt(&rewritten);
                    return;
                }
                // Element-source writeback: `for %h<k>.values { $_ *= 2 }` /
                // `for @a[i].values { ... }`. The plain @/%-source writeback only
                // handles named container variables, so an element source (an
                // Index expression) is rewritten to copy the element into a temp
                // array, iterate that (reusing the array-source writeback), then
                // write the temp back into the element after the loop.
                if let Some(desugared) = self.desugar_for_element_source(stmt) {
                    for s in &desugared {
                        self.compile_stmt(s);
                    }
                    return;
                }
                // Element-source writeback for a bare element source (no
                // `.values`): `for @a[i] { .=Int }` / `for %h<k> { $_ *= 2 }`.
                // See `desugar_for_scalar_element_source`.
                if let Some(desugared) = self.desugar_for_scalar_element_source(stmt) {
                    for s in &desugared {
                        self.compile_stmt(s);
                    }
                    return;
                }
                let block_callable_local = if *uses_block_magic {
                    let closure = if param.is_none() && params.is_empty() && !explicit_zero_params {
                        Expr::AnonSub {
                            body: body.clone(),
                            is_rw: *rw_block,
                            is_block: true,
                        }
                    } else {
                        let (closure_params, closure_param_defs) = if params.is_empty() {
                            (
                                param.iter().cloned().collect(),
                                param_def.iter().cloned().collect(),
                            )
                        } else {
                            (params.clone(), params_def.clone())
                        };
                        Expr::AnonSubParams {
                            params: closure_params,
                            param_defs: closure_param_defs,
                            return_type: None,
                            body: body.clone(),
                            is_rw: *rw_block,
                            is_whatever_code: false,
                        }
                    };
                    self.compile_expr(&closure);
                    let local = self.alloc_fresh_local(&format!(
                        "__mutsu_for_block_callable_{}",
                        self.tmp_counter
                    ));
                    self.tmp_counter += 1;
                    self.code.emit(OpCode::SetLocal(local));
                    Some(local)
                } else {
                    None
                };
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // When there's a single named param (-> $k), store its name as a constant
                // so the VM can bind $k directly without overriding $_
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::str(p.clone())));
                let bind_stmts = Self::build_for_bind_stmts(
                    param,
                    param_def.as_ref(),
                    param_idx,
                    params,
                    params_def,
                );
                // A sigilless raw binding (`-> \v`) aliases the source element
                // directly; in Raku it is writable and modifications propagate
                // back to the source container (`for @a -> \v { v = 99 }` mutates
                // @a, and `for %h.kv -> \k, \v { v = 9 }` / `for %h.values -> \v`
                // write back through the value alias). Treat it like an rw param:
                // don't mark it readonly, and write modifications back.
                let has_sigilless = (**param_def).as_ref().is_some_and(|def| def.sigilless)
                    || params_def.iter().any(|def| def.sigilless);
                // Determine if this for-loop has rw params (via `<->` or `is rw`
                // trait) — for multi-param blocks the per-param defs live in
                // `params_def`, not `param_def`.
                let has_rw = *rw_block
                    || has_sigilless
                    || (**param_def)
                        .as_ref()
                        .is_some_and(|def| def.traits.iter().any(|t| t == "rw"))
                    || params_def
                        .iter()
                        .any(|def| def.traits.iter().any(|t| t == "rw"));
                // `is copy` also makes the param writable (but without writeback)
                let has_copy = (**param_def)
                    .as_ref()
                    .is_some_and(|def| def.traits.iter().any(|t| t == "copy"));
                // Statements that bind the loop parameters (`-> \a, @b, %c`) and
                // mark them read-only. They must run — and the params must be
                // bound — *before* any hoisted body `sub` closes over them, so a
                // sub declared in the body (e.g. a `GENERATE-USAGE` that
                // references the loop's `@expected`) captures this iteration's
                // value, not the previous one. Kept separate from `loop_body` so
                // the hoist is emitted after them (see the compile step below).
                let mut bind_prefix: Vec<Stmt> = Vec::new();
                if !bind_stmts.is_empty() {
                    bind_prefix = bind_stmts;
                    // The loop signature DECLARES these names; the binds below are
                    // plain assignments, so record them for `use strict` (see
                    // `CompiledCode::param_bind_names`).
                    for p in params {
                        if !self.code.param_bind_names.contains(p) {
                            self.code.param_bind_names.push(p.clone());
                        }
                    }
                    // After binding multi-param variables, mark them readonly
                    // (unless the block uses `<->` or `is rw`).
                    // Skip @-sigil and %-sigil params: they bind to a mutable
                    // Array/Hash container, so assignments must be allowed.
                    // Also skip params whose OWN def is writable (`is copy` /
                    // `is rw` / sigilless) — `-> $x is copy, $fn` must leave
                    // $x assignable while $fn stays readonly.
                    if !has_rw && !has_copy && !params.is_empty() {
                        for (i, p) in params.iter().enumerate() {
                            let per_param_writable = params_def.get(i).is_some_and(|d| {
                                d.sigilless || d.traits.iter().any(|t| t == "rw" || t == "copy")
                            });
                            if !p.starts_with('@') && !p.starts_with('%') && !per_param_writable {
                                bind_prefix.push(Stmt::MarkReadonly(p.clone()));
                            }
                        }
                    }
                }
                let arity = if !params.is_empty() {
                    params.len() as u32
                } else {
                    1
                };
                let normalized_iterable = self.normalize_for_iterable(iterable);
                // A `for`-loop handles `is rw` write-back through its own
                // `TagContainerRef` mechanism, so the iterable's synthetic
                // single-element wrap (`for $a` -> `ArrayLiteral([$a])`) must NOT
                // also box `$a` into an aliasing `ContainerRef` cell -- doing so
                // would write that shared cell back into `$a` and create a
                // self-referential cycle (infinite loop on the next read).
                let saved_suppress = self.suppress_list_var_alias;
                self.suppress_list_var_alias = true;
                self.compile_expr(&normalized_iterable);
                self.suppress_list_var_alias = saved_suppress;
                if let Some(source_name) = Self::for_iterable_source_name(iterable) {
                    let source_slot = self.local_map.get(source_name.as_str()).copied();
                    let source_idx = self.code.add_constant(Value::str(source_name));
                    if Self::for_iterable_is_reversed(iterable) {
                        self.code
                            .emit(OpCode::TagContainerRefReversed(source_idx, source_slot));
                    } else {
                        self.code
                            .emit(OpCode::TagContainerRef(source_idx, source_slot));
                    }
                }
                // If the for-loop parameter name already has a local slot
                // (e.g. from a prior `my $i` in an enclosing scope), we must
                // tell the VM so it can keep the local in sync with the env
                // on each iteration and on redo.
                let param_local = param
                    .as_ref()
                    .and_then(|p| self.local_map.get(p.as_str()).copied());
                // A single scalar for-loop param is this compiled code's OWN
                // declaration, not something it could ever need to capture
                // from an enclosing scope -- record it so `compute_free_vars`
                // (opcode.rs) excludes it from `free_var_syms`, mirroring the
                // `my_declared_enum_sym` precedent for a `my enum`'s bareword
                // bindings. Without this, a pure body read of the param name
                // (the loop's own binding write happens inside the ForLoop
                // opcode exec, not a compiled name-write op the free-var scan
                // recognizes) is misclassified as free and rewritten to
                // `GetUpvalue`, which resolves against whatever same-named
                // OUTER lexical this closure happened to capture -- bypassing
                // the loop's per-iteration binding entirely. See
                // todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md
                // (Cro::HTTP::Router::LinkGenerator's `signature-to-sub`).
                // `@`/`%`-sigil and sigilless (`\v`) params are excluded: they
                // don't hit this GetUpvalue path the same way and giving them
                // blanket free-var immunity is unproven for this fix's scope.
                if let Some(p) = param.as_ref()
                    && !p.starts_with(['@', '%', '\\'])
                {
                    self.code
                        .for_loop_param_syms
                        .insert(crate::symbol::Symbol::intern(p));
                }
                // Only an implicit-topic loop rebinds `$_`; see
                // `ForLoopSpec::topic_local`.
                let topic_local = param
                    .is_none()
                    .then(|| self.local_map.get("_").copied())
                    .flatten();
                let kv_mode = has_rw && Self::for_iterable_is_kv(iterable);
                // Names of the loop's multi-params whose value is written back to
                // the source each iteration. Only a *genuinely rw* param writes
                // back: a `<->` block (all params rw), a sigilless raw binding
                // (`\a`), or a param with the `is rw` trait. A plain sigil'd param
                // (`@b` in `-> \a, @b`) is NOT rw just because a sibling forced the
                // loop into rw mode — writing it back would corrupt the source
                // (`for @e -> \a, @b {}` must leave @e untouched). Non-rw slots are
                // kept as "" so the vector stays positionally aligned with the
                // chunk (the writeback skips empty names). In `.kv` mode the key
                // param is read (not written) by the writeback, so it must keep
                // its name — leave that path on the all-names form.
                let rw_param_names: Vec<String> = if has_rw && !params.is_empty() {
                    params
                        .iter()
                        .enumerate()
                        .map(|(i, p)| {
                            let stripped = p.strip_prefix('\\').unwrap_or(p).to_string();
                            let per_param_rw = kv_mode
                                || *rw_block
                                || params_def.get(i).is_some_and(|d| {
                                    d.sigilless || d.traits.iter().any(|t| t == "rw")
                                })
                                // No per-param def (fallback): keep prior behavior
                                // and treat a `\`-prefixed name as rw.
                                || (params_def.get(i).is_none() && p.starts_with('\\'));
                            if per_param_rw {
                                stripped
                            } else {
                                String::new()
                            }
                        })
                        .collect()
                } else {
                    Vec::new()
                };
                let source_var_names = Self::for_iterable_var_names(iterable);
                let source_var_locals = self.for_source_var_locals(&source_var_names);
                let source_container_local = Self::for_iterable_source_name(iterable)
                    .and_then(|name| self.local_map.get(&name).copied());
                // The local slot each multi-param bind will land in, captured
                // BEFORE `bind_prefix` is compiled (see the field doc on
                // `ForLoopSpec::multi_param_locals`): `build_for_bind_stmts`
                // binds via `Stmt::Assign`, which never allocates a new slot —
                // it resolves to whatever `local_map` already maps the name to
                // right now, or falls through to a global write if there is
                // none. Reading `local_map` at this exact point mirrors that
                // resolution exactly.
                let multi_param_locals: Vec<Option<u32>> = params
                    .iter()
                    .map(|p| {
                        let bare = p.strip_prefix('\\').unwrap_or(p);
                        self.local_map.get(bare).copied()
                    })
                    .collect();
                // When the block parameter has a type constraint other than Mu
                // or Junction, junction items should be autothreaded (expanded
                // into their eigenstates).
                let autothread_junctions = match param_def.as_ref() {
                    Some(def) => match def.type_constraint.as_deref() {
                        None | Some("Mu") | Some("Junction") => false,
                        Some(_) => true,
                    },
                    // No param_def means default (Mu) — no autothreading
                    None => false,
                };
                let loop_idx =
                    self.code
                        .emit(OpCode::ForLoop(Box::new(crate::opcode::ForLoopSpec {
                            param_idx,
                            param_local,
                            topic_local,
                            source_container_local,
                            body_end: 0,
                            block_callable_local,
                            label: label.clone(),
                            arity,
                            collect: false,
                            threaded: matches!(
                                *mode,
                                crate::ast::ForMode::Race | crate::ast::ForMode::Hyper
                            ),
                            // is_rw: param is writable (don't mark readonly)
                            is_rw: has_rw || has_copy,
                            // do_writeback: actually write back modifications to source container
                            do_writeback: has_rw && !has_copy,
                            rw_param_names,
                            kv_mode,
                            source_var_names,
                            source_var_locals,
                            autothread_junctions,
                            explicit_zero_params: *explicit_zero_params,
                            multi_param_names: params
                                .iter()
                                .map(|p| p.strip_prefix('\\').unwrap_or(p).to_string())
                                .collect(),
                            multi_param_locals,
                            param_type_constraint: param_def
                                .as_ref()
                                .as_ref()
                                .and_then(|d| d.type_constraint.clone()),
                            multi_param_type_constraints: (0..params.len())
                                .map(|i| params_def.get(i).and_then(|d| d.type_constraint.clone()))
                                .collect(),
                            loop_var_wraps_element: Self::for_iterable_wraps_pair(iterable),
                            values_mode: Self::for_iterable_is_values_alias(iterable),
                            direct_smartmatch: Self::for_direct_smartmatch(iterable),
                            single_array_source: Self::for_single_array_source(iterable),
                            single_array_source_local: self.for_single_array_source_local(
                                &Self::for_single_array_source(iterable),
                            ),
                            body_declares_routines: Self::stmts_declare_routines(&loop_body),
                        })));
                // Register sigilless for-params (`-> \v`, `-> \k, \v`) as
                // sigilless locals while compiling the body so postfix/prefix
                // `++`/`--` on the bare word (`v--`, `++v`) resolve to an
                // in-place PostDecrement/etc. on the bound env var rather than
                // the `__mutsu_incdec_nomatch` fallback. They are NOT readonly
                // (rw aliases), so we only add them to the set, not mark them.
                let sigilless_param_names: Vec<String> = if has_sigilless {
                    let mut names = Vec::new();
                    let single_sigilless = (**param_def).as_ref().is_some_and(|def| def.sigilless);
                    if let Some(p) = param.as_ref().filter(|_| single_sigilless) {
                        names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
                    }
                    for (p, def) in params.iter().zip(params_def.iter()) {
                        if def.sigilless {
                            names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
                        }
                    }
                    names
                } else {
                    Vec::new()
                };
                let newly_registered: Vec<String> = sigilless_param_names
                    .iter()
                    .filter(|n| self.sigilless_locals.insert((*n).clone()))
                    .cloned()
                    .collect();
                // Bind the loop parameters first, then hoist the body's routine
                // declarations so they capture the freshly-bound params. Hoisting
                // makes a `sub` declared later in the body visible from its
                // beginning (Raku scopes named subs to their whole lexical block);
                // the paired registry snapshot/restore in the VM (gated on
                // `body_declares_routines`) keeps them from leaking past the loop.
                for s in &bind_prefix {
                    self.compile_stmt(s);
                }
                self.hoist_sub_decls(&loop_body, true);
                // A `for` body is its own Raku call frame; count it so a
                // `callframe`/`caller` inside sees the enclosing routine one
                // level further up (see `callframe_block_depth`).
                self.callframe_block_depth += 1;
                // Only the statement-MODIFIER form's sole block is the loop's
                // own body (cloned once per statement, state persists). A sole
                // block inside a prefix `for` body (`for ^3 { { state ... } }`)
                // is a NESTED bare block that re-clones per iteration, so its
                // per-execution ResetStateLocals must stay (raku prints 1 1 1
                // there — t/state-var-per-block-clone.t test 5).
                self.suppress_loop_block_state_reset =
                    *is_statement_modifier && Self::loop_body_is_sole_block(&loop_body);
                // ...and by the same token that sole block is the loop's own
                // block, supplied one element per iteration -- not a nested
                // zero-argument one (ADR-0048 D3/D6). Re-noted here because
                // `expand_loop_phasers` rebuilt the body list.
                if *is_statement_modifier {
                    self.note_construct_body_block_stmts(&loop_body);
                }
                self.compile_scope_restored_loop_body(&loop_body);
                self.callframe_block_depth -= 1;
                for n in &newly_registered {
                    self.sigilless_locals.remove(n);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
                // Restore the single named loop param after the post (LAST)
                // phasers ran. The ForLoop opcode deferred this restore (pushing
                // its saved binding) so the phasers could still see the param at
                // its final value. Emit only when a single non-@/% named param
                // exists, mirroring the VM's save condition so push/pop balance.
                if param
                    .as_ref()
                    .is_some_and(|p| !p.starts_with('@') && !p.starts_with('%'))
                {
                    self.code.emit(OpCode::RestoreForParam);
                }
            }
            // ADR-0048 Phase 2: `loop {}` (headerless and C-style) does not
            // take a signature in raku. `repeat: true` (`repeat {}
            // while/until`) is deliberately EXCLUDED here — it is a
            // different, signature-capable construct (D4/Phase 4; see the
            // oracle's doc comment on `Stmt::Loop { repeat: true, .. }` in
            // `src/ast.rs`) — matching real `raku`, which does NOT reject a
            // placeholder in a `repeat` body
            // (`roast/S04-statements/repeat.t`'s "placeholders and 'repeat
            // while' mix" subtest). Guard placed before both real
            // `Stmt::Loop` arms (this one and the repeat-loop arm further
            // below), mirroring the existing `ClassDecl`/`RoleDecl` pattern.
            Stmt::Loop {
                body,
                repeat: false,
                ..
            } if self.emit_block_placeholder_die(body) => {}
            // C-style loop (non-repeat, no phasers)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if !*repeat => {
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                // Compile init statement (if any) before the loop opcode
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // Layout: [CStyleLoop] [cond..] [body..] [step..]
                let loop_idx = self.code.emit(OpCode::CStyleLoop {
                    cond_end: 0,
                    step_start: 0,
                    body_end: 0,
                    label: label.clone(),
                    collect: false,
                });
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_condition_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                self.code.patch_cstyle_cond_end(loop_idx);
                // Compile body. A sole `{ ... }` here is a nested bare block
                // (C-style `loop` has no statement-modifier form), so its
                // `state` restarts per iteration — no reset suppression.
                self.compile_scope_restored_loop_body(&loop_body);
                self.code.patch_cstyle_step_start(loop_idx);
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::Call { name, args } => {
                // Check for invocant colon syntax: foo($obj:) → $obj.foo()
                if let Some(CallArg::Invocant(_)) = args.first() {
                    let invocant_expr = match &args[0] {
                        CallArg::Invocant(e) => e.clone(),
                        _ => unreachable!(),
                    };
                    let method_args: Vec<Expr> = args[1..]
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(e) => Some(e.clone()),
                            _ => None,
                        })
                        .collect();
                    let method_call = Expr::MethodCall {
                        target: Box::new(invocant_expr),
                        name: *name,
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    };
                    self.compile_expr(&method_call);
                    // Sink context: a method-call statement (`foo($obj:);`,
                    // i.e. `$obj.foo();`) sinks its value, and sinking an
                    // unhandled Failure throws. `true` = the value is a fresh
                    // rvalue that may run a user-defined `sink` method.
                    self.code.emit(OpCode::SinkPop(true, true));
                    return;
                }

                let name_str = name.resolve();

                // A `my &f` (or `&f`/`:&f` Callable parameter) binding visible
                // here SHADOWS a same-named builtin/registered sub for a
                // statement-position bare call too — the same shadowing
                // `compile_expr_call_inner` already applies at expression
                // position (`self.amp_binding_in_active_scope`). Without this,
                // the `ExecCall` opcode below dispatches purely by name at
                // runtime, with no notion of a local Callable binding, so a
                // mid-body (non-final) statement call could reach a builtin or
                // control-flow implementation of the same name instead of the
                // lexical — e.g. `emit()`/`done()` followed by more statements,
                // with a lexical `&emit`/`&done` in scope, hit the real
                // supply/react control-flow builtins. Route it through the
                // expression-call path instead, which already resolves the
                // CodeVar. See
                // todo/tickets/code-lexical-does-not-shadow-a-builtin.md.
                if self.amp_binding_in_active_scope(&name_str) {
                    let call_expr = Expr::Call {
                        name: *name,
                        args: Self::call_args_to_expr_args(args),
                    };
                    self.compile_expr(&call_expr);
                    // Sink context: a bare call statement sinks its value (see
                    // the identical `SinkPop(false, true)` below for the
                    // normalized mutating-call path).
                    self.code.emit(OpCode::SinkPop(false, true));
                    return;
                }

                let rewritten_args = Self::rewrite_stmt_call_args(&name_str, args);
                let positional_only = rewritten_args
                    .iter()
                    .all(|arg| matches!(arg, CallArg::Positional(_)));

                // A slip argument does not stop the mutating-listop rewrite: it
                // round-trips to the expression form as `|EXPR`
                // (`Expr::Unary { op: Pipe }`), exactly what the expression
                // parser produces for the value-position spelling. Without this
                // `push(@a, 1, |@rest);` as a *statement* fell through to the
                // generic `ExecCallPairs` dispatch, which has no `push` routine
                // to resolve and died with "Unknown call: push" — while the same
                // call in value position (`my $r = push(...)`) worked. Limited to
                // the fixed listop set; an imported routine keeps the stricter
                // positional-only condition.
                let listop_slip_ok = matches!(
                    name_str.as_str(),
                    "push" | "unshift" | "append" | "prepend" | "splice"
                ) && rewritten_args
                    .iter()
                    .all(|a| matches!(a, CallArg::Positional(_) | CallArg::Slip(_)));

                // Normalize mutating/structural call statements through Expr::Call
                // so they reuse call rewrites and method-based mutation paths.
                if (positional_only || listop_slip_ok)
                    && Self::is_normalized_stmt_call_name(&name_str)
                {
                    let expr_args: Vec<Expr> = rewritten_args
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(expr) => Some(expr.clone()),
                            CallArg::Slip(expr) => Some(Expr::Unary {
                                op: crate::token_kind::TokenKind::Pipe,
                                expr: Box::new(expr.clone()),
                            }),
                            _ => None,
                        })
                        .collect();
                    let call_expr = Expr::Call {
                        name: *name,
                        args: expr_args,
                    };
                    self.compile_expr(&call_expr);
                    // Sink context: a bare call statement sinks its value, so a
                    // returned unhandled Failure throws (e.g. `pop @a;` /
                    // `shift @a;` on an empty array — Raku's X::Cannot::Empty).
                    // `false` = a function-call return is not auto-`sink`ed
                    // (Raku keeps it container-wrapped); only Failure/lazy
                    // handling applies. Matches the method form `@a.pop;`.
                    self.code.emit(OpCode::SinkPop(false, true));
                    return;
                }

                // Statement-level call: compile positional args only.
                // Fall back if named args or raw-expression args remain.
                if positional_only
                    && rewritten_args
                        .iter()
                        .all(|arg| matches!(arg, CallArg::Positional(_)))
                {
                    let arity = rewritten_args.len() as u32;
                    let positional_exprs: Vec<Expr> = rewritten_args
                        .iter()
                        .filter_map(|arg| match arg {
                            CallArg::Positional(expr) => Some(expr.clone()),
                            _ => None,
                        })
                        .collect();
                    let arg_sources_idx = self.add_arg_sources_constant(&positional_exprs);
                    for arg in &rewritten_args {
                        if let CallArg::Positional(expr) = arg {
                            self.compile_call_arg(expr);
                        }
                    }
                    let name_idx = self.code.add_constant(Value::str(name.resolve()));
                    self.code.emit(OpCode::ExecCall {
                        name_idx,
                        arity,
                        arg_sources_idx,
                    });
                    return;
                }

                // Statement-level call with named args: compile values and encode
                // named args as Pair(name => value), then dispatch without stmt_pool.
                //
                // A closure literal NAMED-argument value escapes exactly as it
                // does for a plain call's named-args branch
                // (`compile_expr_call_inner`, and the identical fix in
                // `compile_tail_stmt_call_value`): the callee may store it
                // rather than invoke it immediately, and this stmt-call shape
                // (a listop-style call whose callee is not statically known,
                // e.g. an imported routine — see `Stmt::Call`) is otherwise
                // indistinguishable from a plain call at the syntax level.
                // Without this, a closure literal's captured-and-mutated free
                // variables never get boxed into a shared cell, so a
                // same-named parameter in the callee's own call chain can
                // shadow the closure's own captured lexical when it is later
                // invoked from a nested block
                // (todo/deep/closure-capture-shadowed-by-colliding-callee-parameter.md).
                //
                // Positional args here deliberately keep `compile_call_arg`'s
                // unconditional non-escaping treatment: unlike the named-arg
                // case, marking a positional closure literal (e.g.
                // `lives-ok { ... }, $desc` — rewritten to an anon sub before
                // this loop runs, so it still matches `is_closure_literal_arg`)
                // escaping here regressed `t/bind-alias-chain.t`, so this
                // narrower fix only touches the shape the bug report is about.
                for arg in &rewritten_args {
                    match arg {
                        CallArg::Positional(expr) => self.compile_call_arg(expr),
                        CallArg::Named {
                            name,
                            value: Some(expr),
                        } => {
                            self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                            let escaping = Self::is_closure_literal_arg(expr);
                            self.with_escape(escaping, |s| s.compile_expr(expr));
                            self.code.emit(OpCode::MakeNamedArg);
                        }
                        CallArg::Named { name, value: None } => {
                            self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                            self.compile_expr(&Expr::Literal(Value::TRUE));
                            self.code.emit(OpCode::MakeNamedArg);
                        }
                        // `|EXPR` interpolates into the argument list: MakeSlip
                        // builds the Slip, which spreads when bound.
                        CallArg::Slip(expr) => {
                            self.compile_expr(expr);
                            self.code.emit(OpCode::MakeSlip);
                        }
                        CallArg::Invocant(_) => unreachable!(),
                    }
                }
                let name_idx = self.code.add_constant(Value::str(name.resolve()));
                let arg_sources_idx = self.add_call_arg_sources_constant(&rewritten_args);
                self.code.emit(OpCode::ExecCallPairs {
                    name_idx,
                    arity: rewritten_args.len() as u32,
                    arg_sources_idx,
                    keep_value: false,
                });
            }
            // Loop control
            Stmt::Goto(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Goto);
            }
            Stmt::Label { name, stmt } => {
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                self.code.emit(OpCode::Label(name_idx));
                self.compile_stmt(stmt);
            }
            Stmt::Last(label) => {
                self.code.emit(OpCode::Last(label.clone()));
            }
            Stmt::Next(label) => {
                self.code.emit(OpCode::Next(label.clone()));
            }
            Stmt::Redo(label) => {
                self.code.emit(OpCode::Redo(label.clone()));
            }
            Stmt::Return(expr) => {
                // A returned closure escapes the routine frame (escape analysis).
                self.with_escape(true, |c| c.compile_expr(expr));
                if self.is_routine {
                    self.code.emit(OpCode::Return);
                } else {
                    self.code.emit(OpCode::ReturnFromNonRoutine(
                        self.lexically_in_routine,
                        self.eval_context_dead_routine,
                    ));
                }
            }
            Stmt::Die(expr) => {
                self.compile_expr(expr);
                self.code.emit(OpCode::Die);
            }
            Stmt::Fail(expr) => {
                // A failed value is stored/propagated -> closure escapes.
                self.with_escape(true, |c| c.compile_expr(expr));
                self.code.emit(OpCode::Fail);
            }
            Stmt::Proceed => {
                self.code.emit(OpCode::Proceed);
            }
            Stmt::Succeed => {
                self.code.emit(OpCode::Succeed);
            }
            Stmt::ReactDone => {
                self.code.emit(OpCode::ReactDone);
            }
            Stmt::SupplyBodyDone => {
                self.code.emit(OpCode::SupplyBodyDone);
            }
            // MatchAssign (~~=): coerce value to string
            Stmt::Assign {
                name,
                expr,
                op: AssignOp::MatchAssign,
            } if name != "*PID" => {
                self.with_escape(true, |c| c.compile_expr(expr));
                self.code.emit(OpCode::StrCoerce);
                self.emit_set_named_var(name);
            }
            Stmt::Assign { .. } => {
                self.code.emit(OpCode::AssignReadOnly);
            }
            // Given/When/Default
            Stmt::Given {
                topic,
                body,
                is_statement_modifier,
            } => {
                // A pointy `-> $_ is copy` block starts with a parser-generated
                // lexical declaration carrying the `__pointy_copy` marker: the
                // topic becomes a fresh, writable copy with NO writeback to the
                // source. Detect that declaration so the topic is not marked
                // read-only (`given 42 -> $_ is copy` must allow `$_ = ...`) and
                // so a bare-variable topic is not tagged for writeback
                // (`given $x -> $_ is copy { $_ = ... }` leaves `$x` untouched).
                let is_copy_topic = body.first().is_some_and(|stmt| {
                    matches!(
                        stmt,
                        Stmt::VarDecl { custom_traits, .. }
                            if custom_traits.iter().any(|(name, _)| name == "__pointy_copy")
                    )
                });
                // An lvalue container *element* topic (`given %h<k>` /
                // `given @a[i]`) aliases that element rw: both `$_ = ...` and
                // container mutations (`.push`) propagate to the element. Push
                // the element value and tag the (container, index) source so the
                // body's final `$_` is written back. `topic_readonly` is false.
                let element_source = match topic {
                    Expr::Index {
                        target,
                        index,
                        is_positional,
                    } => Self::container_var_name(target)
                        // The element-source writeback optimization looks the
                        // container up by name in the locals store. An instance
                        // attribute (`%!h`, `@!a`, twigil `!`/`.`) lives in the
                        // instance attribute store, not in locals, so the lookup
                        // would read an empty container and bind `$_` to Nil.
                        // Fall through to evaluating the element value directly
                        // (read-only, but correct) for attribute containers.
                        .filter(|c| {
                            let after_sigil = c.strip_prefix(['$', '@', '%']).unwrap_or(c);
                            !after_sigil.starts_with(['!', '.'])
                        })
                        .map(|c| (c, index, *is_positional)),
                    _ => None,
                };
                let topic_readonly;
                if let Some((container, index, is_positional)) = element_source {
                    self.compile_expr(index);
                    let container_idx = self.code.add_constant(Value::str(container));
                    self.code.emit(OpCode::TagElementSource {
                        container_idx,
                        positional: is_positional,
                    });
                    topic_readonly = false;
                } else {
                    self.compile_expr(topic);
                    // `given my $x = EXPR` (a scalar declaration used as the topic)
                    // aliases the freshly-declared `$x` rw, exactly like `given $x`,
                    // so `$_ = ...` / `s///` inside the block write back to `$x`.
                    // The declaration is wrapped in a `DoStmt`; the VarDecl name has
                    // no sigil for scalars (arrays/hashes carry `@`/`%`).
                    let topic_decl_scalar = match topic {
                        Expr::DoStmt(inner) => match inner.as_ref() {
                            Stmt::VarDecl { name, .. }
                                if !name.starts_with('@')
                                    && !name.starts_with('%')
                                    && !name.starts_with('&') =>
                            {
                                Some(name.clone())
                            }
                            _ => None,
                        },
                        _ => None,
                    };
                    // `is copy` makes a detached copy, so suppress the
                    // source-writeback tag even for a bare-variable topic.
                    let source_name = if is_copy_topic {
                        None
                    } else {
                        match topic {
                            Expr::Var(name) => Some(name.clone()),
                            Expr::ArrayVar(name) => Some(format!("@{}", name)),
                            Expr::HashVar(name) => Some(format!("%{}", name)),
                            _ => topic_decl_scalar.clone(),
                        }
                    };
                    if let Some(source_name) = source_name {
                        let source_slot = self.local_map.get(source_name.as_str()).copied();
                        let name_idx = self.code.add_constant(Value::str(source_name));
                        self.code
                            .emit(OpCode::TagContainerRef(name_idx, source_slot));
                    }
                    // The topic is read-only unless it is a bare scalar variable
                    // (`given $x` aliases `$x` rw), a scalar declaration topic
                    // (`given my $x = ...`), or an `is copy` writable copy.
                    // `given @a` / `given 42` / `given expr()` are read-only (Raku
                    // errors on `$_ = ...`).
                    topic_readonly = !is_copy_topic
                        && !matches!(topic, Expr::Var(_))
                        && topic_decl_scalar.is_none();
                }
                // A pointy block (`given @a -> @p { ... }`) starts with a
                // parser-generated synthetic declaration containing MarkBind.
                // Record that bound parameter so topic-source writeback reads its
                // final value (e.g. after `@p.push`) instead of `$_`, propagating
                // the mutation back to the source. `is copy` carries a declaration
                // marker instead, so it is not detected here and does not write
                // back.
                // A native-typed pointy param (`given $x -> int $v is rw {...}`)
                // cannot use `:=` (see `pointy_topic_bind`'s native branch), so
                // it carries `__pointy_native_param` instead of `MarkBind` —
                // either bare (`is rw`) or wrapped in a `SyntheticBlock` with a
                // trailing `MarkReadonly` (the default, readonly case).
                let is_pointy_native_decl = |s: &Stmt| {
                    matches!(s, Stmt::VarDecl { custom_traits, .. }
                        if custom_traits.iter().any(|(t, _)| t == "__pointy_native_param"))
                };
                let pointy_param_name = match body.first() {
                    Some(Stmt::SyntheticBlock(inner))
                        if inner
                            .iter()
                            .any(|s| matches!(s, Stmt::MarkBind) || is_pointy_native_decl(s)) =>
                    {
                        inner.iter().find_map(|s| match s {
                            Stmt::VarDecl { name, .. }
                                if !name.starts_with('!')
                                    && !name.starts_with('.')
                                    && !name.starts_with('&') =>
                            {
                                Some(name.clone())
                            }
                            _ => None,
                        })
                    }
                    Some(s @ Stmt::VarDecl { name, .. }) if is_pointy_native_decl(s) => {
                        Some(name.clone())
                    }
                    _ => None,
                };
                // A scalar placeholder in the body is the given/with BLOCK's
                // parameter, bound to the topic (`with 2 { $^a == 3 ?? … }`
                // sees 2). The topic value is on the stack here; keep a copy
                // for the binding, mirroring the If arm's cond placeholder.
                //
                // A `given` STATEMENT MODIFIER introduces no block, so a
                // placeholder in its body belongs to the enclosing routine and
                // is already bound as one of its parameters (see the matching
                // `is_statement_modifier` arm in `collect_ph_stmt_shallow`).
                // Rebinding it to the topic here made
                // `sub ROL64 { ($^a … $_ …) given $^n%64 }` read the topic for
                // `$^a` instead of the first argument.
                if !*is_statement_modifier {
                    // ADR-0048 D3's shared bind. `Dup` only when a scalar
                    // placeholder will actually consume the copy -- the topic
                    // itself must stay on the stack for `OpCode::Given`.
                    if Self::inlined_body_binds_supplied_value(body) {
                        self.code.emit(OpCode::Dup);
                    }
                    self.emit_inlined_body_placeholder_binds(body, ArgSupply::Topic);
                }
                let pointy_param_idx =
                    pointy_param_name.map(|name| self.code.add_constant(Value::str(name)));
                let given_idx = self.code.emit(OpCode::Given {
                    body_end: 0,
                    topic_readonly,
                    pointy_param_idx,
                });
                let block_local_idx = (!*is_statement_modifier
                    && Self::branch_declares_block_local(body))
                .then(|| {
                    self.code.emit(OpCode::BlockLocalScope {
                        body_end: 0,
                        succeed_boundary: false,
                    })
                });
                let saved_scope =
                    (!*is_statement_modifier).then(|| self.push_dynamic_scope_lexical());
                if Self::has_block_leave_worthy_phasers(body) {
                    // Unlike `Stmt::If`'s body, a `given` body was compiled
                    // by iterating and compiling each statement in place —
                    // an un-lowered `Stmt::Phaser { kind: Leave, .. }` alone
                    // compiles to a no-op, so its LEAVE never fired. Mirrors
                    // the `Stmt::If` arm's own check. Deliberately
                    // `has_block_leave_worthy_phasers`, not
                    // `has_block_enter_leave_phasers` — the loop-phaser
                    // lowering (`helpers_phasers.rs`) synthesizes a `given
                    // $topic { POST { ... } }`/`PRE` wrapper whose body is
                    // solely a re-wrapped `Stmt::Phaser` node; routing that
                    // phaser-only body through `compile_phaser_block_scope`
                    // left its topic unset, breaking POST/PRE inside loops.
                    //
                    // This is a *statement*-context `given` (an expression-
                    // context one, e.g. `do given ... { ... }`, compiles
                    // through a different, `Push`-mode path) sharing the
                    // enclosing frame's `$_` register. `PhaserBlockResult::
                    // Discard` (not `ReturnViaTopic`, unlike the `Stmt::If`
                    // arm before this same fix): routing the body's own
                    // trailing value through `SetTopic` reassigned `$_` to
                    // it, clobbering the topic a LEAVE phaser needs (`given
                    // open $path, :w { LEAVE .close; say $_ }` made `.close`
                    // see `say`'s `Bool` result instead of the file handle,
                    // breaking roast/S32-io/open.t and spurt.t). `Discard`
                    // still lets the value survive on the stack through
                    // LEAVE/KEEP/UNDO/POST (so their own checks/reads still
                    // see it), then pops it once at the very end instead of
                    // ever routing it through `$_` -- verified against real
                    // raku for both statement-context `given` (a trailing
                    // sink-context warning, same as raku) and `do given`
                    // expression context (the value still comes through
                    // correctly, via the separate `Push`-mode path).
                    self.compile_phaser_block_scope(body, PhaserBlockResult::Discard);
                } else if Self::has_catch_or_control(body) {
                    self.compile_implicit_try(body);
                    self.code.emit(OpCode::Pop);
                } else {
                    for (i, s) in body.iter().enumerate() {
                        let is_last = i == body.len() - 1;
                        if is_last {
                            if !self.compile_when_tail_stmt(s) {
                                self.compile_stmt(s);
                            }
                        } else {
                            self.compile_stmt(s);
                        }
                    }
                }
                if let Some(saved) = saved_scope {
                    self.pop_dynamic_scope_lexical(saved);
                }
                if let Some(idx) = block_local_idx {
                    self.code.patch_block_local_body_end(idx);
                }
                self.code.patch_body_end(given_idx);
            }
            Stmt::When { cond, body } => {
                self.compile_expr(cond);
                let when_idx = self.code.emit(OpCode::When { body_end: 0 });
                let block_local_idx = Self::branch_declares_block_local(body).then(|| {
                    self.code.emit(OpCode::BlockLocalScope {
                        body_end: 0,
                        succeed_boundary: false,
                    })
                });
                let saved_scope = self.push_dynamic_scope_lexical();
                // ADR-0048 D3: a `when` body is a Block raku invokes with ZERO
                // arguments (`{ when 5 { $^c } }.arity` is 0), so any placeholder
                // it declares is an unsatisfied parameter. Emitted INSIDE the
                // `When` region: a non-matching `when` never invokes its body and
                // must not raise (`given 5 { when 6 { $^c }; say "no match" }`
                // prints "no match" in raku).
                self.emit_inlined_body_placeholder_binds(body, ArgSupply::None);
                for (i, s) in body.iter().enumerate() {
                    let is_last = i == body.len() - 1;
                    if is_last {
                        if !self.compile_when_tail_stmt(s) {
                            self.compile_stmt(s);
                        }
                    } else {
                        self.compile_stmt(s);
                    }
                }
                self.pop_dynamic_scope_lexical(saved_scope);
                if let Some(idx) = block_local_idx {
                    self.code.patch_block_local_body_end(idx);
                }
                self.code.patch_body_end(when_idx);
            }
            // ADR-0048 Phase 2: `default {}` does not take a signature in
            // raku (`$^c` used directly inside one is `X::Placeholder::Block`,
            // even though `default` itself binds the topic). Guard placed
            // before the real `Stmt::Default` arm below, mirroring the
            // existing `ClassDecl`/`RoleDecl` pattern.
            Stmt::Default(body) if self.emit_block_placeholder_die(body) => {}
            Stmt::Default(body) => {
                let default_idx = self.code.emit(OpCode::Default { body_end: 0 });
                let block_local_idx = Self::branch_declares_block_local(body).then(|| {
                    self.code.emit(OpCode::BlockLocalScope {
                        body_end: 0,
                        succeed_boundary: false,
                    })
                });
                let saved_scope = self.push_dynamic_scope_lexical();
                if Self::has_catch_or_control(body) {
                    self.compile_implicit_try(body);
                    self.code.emit(OpCode::Pop);
                } else {
                    for (i, s) in body.iter().enumerate() {
                        let is_last = i == body.len() - 1;
                        if is_last {
                            if !self.compile_when_tail_stmt(s) {
                                self.compile_stmt(s);
                            }
                        } else {
                            self.compile_stmt(s);
                        }
                    }
                }
                self.pop_dynamic_scope_lexical(saved_scope);
                if let Some(idx) = block_local_idx {
                    self.code.patch_block_local_body_end(idx);
                }
                self.code.patch_body_end(default_idx);
            }
            // Repeat loop (repeat while / repeat until)
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } if *repeat => {
                let (pre_stmts, loop_body, post_stmts) =
                    self.expand_loop_phasers(body, label.as_deref());
                if let Some(init_stmt) = init {
                    self.compile_stmt(init_stmt);
                }
                for s in &pre_stmts {
                    self.compile_stmt(s);
                }
                // Layout: [RepeatLoop] [body..] [cond..]
                let loop_idx = self.code.emit(OpCode::RepeatLoop {
                    cond_end: 0,
                    body_end: 0,
                    label: label.clone(),
                });
                // Compile body. The parser inlines the `repeat { ... }` block's
                // statements directly into `body`, so a sole `{ ... }` here is
                // a NESTED bare block that re-clones per iteration — its
                // `state` restarts (raku: 1 1 1), no reset suppression.
                self.compile_scope_restored_loop_body(&loop_body);
                self.code.patch_repeat_cond_end(loop_idx);
                // Compile condition (or push True if none)
                if let Some(cond_expr) = cond {
                    self.compile_condition_expr(cond_expr);
                } else {
                    self.code.emit(OpCode::LoadTrue);
                }
                // Compile step (if any)
                if let Some(step_expr) = step {
                    self.compile_expr(step_expr);
                    self.code.emit(OpCode::Pop);
                }
                self.code.patch_loop_end(loop_idx);
                for s in &post_stmts {
                    self.compile_stmt(s);
                }
            }
            Stmt::Loop { .. } => unreachable!("loop repeat flag is exhaustive"),
            // --- No-ops: these statements are handled elsewhere ---
            // CATCH/CONTROL are extracted by compile_try/compile_body_with_implicit_try
            Stmt::Catch(_) | Stmt::Control(_) => {}
            // HasDecl outside class context.
            Stmt::HasDecl { is_our, is_my, .. } => {
                // `our $.x` / `my $.x` in the mainline is not a fatal error in
                // Raku; it merely warns that generating an accessor method here
                // is useless (there is no package to attach it to). Only the
                // `has $.x` form (no `our`/`my`) is fatal.
                if *is_our || *is_my {
                    let warn_call = Expr::Call {
                        name: Symbol::intern("warn"),
                        args: vec![Expr::Literal(Value::str(
                            "Useless generation of accessor method in mainline".to_string(),
                        ))],
                    };
                    self.compile_expr(&warn_call);
                    self.code.emit(OpCode::Pop);
                    return;
                }
                // `Stmt::HasDecl::name` is already the bare (twigil-free) name
                // and `sigil` is always `$`/`@`/`%` (the parser never produces
                // `.`/`!`), so `CompiledAttrDecl::from_stmt`'s fields match
                // this arm's historical `bare`/`sigil_ch` derivation exactly.
                let decl = crate::opcode::CompiledAttrDecl::from_stmt(
                    stmt,
                    crate::opcode::AttrDeclChunks::default(),
                );
                let twigil = if decl.is_public { "." } else { "!" };
                let full_name = format!("{}{}{}", decl.sigil, twigil, decl.name);
                let mut attrs = std::collections::HashMap::new();
                let err = if let Some(pkg_kind) = self.current_package_kind {
                    // Inside a `module`/`package` body: a package cannot hold
                    // attributes — X::Attribute::Package.
                    let kind_str = pkg_kind.as_str();
                    let message = format!(
                        "A {} cannot have attributes, but you tried to declare '{}'",
                        kind_str, full_name
                    );
                    attrs.insert("name".to_string(), Value::str(full_name));
                    attrs.insert("package-kind".to_string(), Value::str(kind_str.to_string()));
                    attrs.insert("message".to_string(), Value::str(message));
                    Value::make_instance(Symbol::intern("X::Attribute::Package"), attrs)
                } else {
                    // Mainline: no enclosing package at all — X::Attribute::NoPackage.
                    let message = format!(
                        "You cannot declare attribute '{}' here; maybe you'd like a class or a role?",
                        full_name
                    );
                    attrs.insert("name".to_string(), Value::str(full_name));
                    attrs.insert("message".to_string(), Value::str(message));
                    Value::make_instance(Symbol::intern("X::Attribute::NoPackage"), attrs)
                };
                // A `has` reaching the VM only arises from mainline / EVAL'd
                // source (a `has` in a normal class body is collected
                // declaratively by `register_class_decl`, never compiled). Emit a
                // runtime op that, when a class is currently being defined
                // (`class Foo { BEGIN EVAL q[has $.x] }`), registers the
                // attribute onto that class; otherwise it throws the error above.
                let spec = crate::opcode::RuntimeHasDeclSpec { decl, error: err };
                self.code.emit(OpCode::RuntimeHasDecl(Box::new(spec)));
            }
            // DoesDecl/TrustsDecl outside class context are no-ops
            Stmt::DoesDecl { .. } | Stmt::TrustsDecl { .. } => {}

            // --- Take (gather/take) ---
            Stmt::Take(expr, is_rw) => {
                // `take |EXPR` is a call with a flattened argument list, not
                // `take` of a runtime Slip. Route this spelling through the
                // ordinary call path so multiple positional arguments are
                // bundled by the `take` builtin into one List item. The direct
                // Take opcode must remain for `take EXPR`, where a Slip is
                // intentionally flattened into the gather.
                if !*is_rw
                    && matches!(
                        expr,
                        Expr::Unary {
                            op: crate::token_kind::TokenKind::Pipe,
                            ..
                        }
                    )
                {
                    let call = Expr::Call {
                        name: Symbol::intern("take"),
                        args: vec![expr.clone()],
                    };
                    self.compile_expr(&call);
                    self.code.emit(OpCode::Pop);
                    return;
                }
                if *is_rw {
                    // `take-rw <lvalue>`: capture the source container (a shared
                    // `ContainerRef` cell), not a snapshot, so the gathered value
                    // keeps container identity with the original (`=:=`). Compile
                    // the operand exactly like a `:=` bind RHS: `scalar_bind_autovivify`
                    // makes an element subscript (`@a[i][j]`) yield the promoted
                    // cell; `bind_terminal` marks the leaf so a scalar element is
                    // boxed. A leading `// next` guard preserves the cell because
                    // `//` returns its (peeked) left operand unchanged when defined.
                    let saved_av = self.scalar_bind_autovivify;
                    let saved_term = self.bind_terminal;
                    self.scalar_bind_autovivify = true;
                    self.bind_terminal = true;
                    self.compile_expr(expr);
                    self.scalar_bind_autovivify = saved_av;
                    self.bind_terminal = saved_term;
                } else {
                    self.compile_expr(expr);
                }
                self.code.emit(OpCode::Take);
            }

            // ADR-0048 Phase 2: `react {}` does not take a signature in raku.
            Stmt::React { body } if self.emit_block_placeholder_die(body) => {}
            // --- React: event loop scope ---
            Stmt::React { body } => {
                let idx = self.code.emit(OpCode::ReactScope { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(idx);
            }

            // ADR-0048 Phase 2: `module`/`package`/`grammar` bodies do not
            // take a signature in raku (unlike `role`, D7/Phase 5).
            Stmt::Package { body, .. } if self.emit_block_placeholder_die(body) => {}
            // --- Package scope ---
            Stmt::Package {
                name,
                body,
                kind,
                is_unit,
                is_my,
            } => {
                let qualified_name = self.qualify_package_name(&name.resolve());
                // Detect stub body: `module Foo { ... }` — body is a stub operator
                // Filter out SetLine when checking, since the parser now emits
                // line tracking statements in all block bodies.
                let non_setline_body: Vec<_> = body
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)))
                    .collect();
                let is_stub_body = non_setline_body.len() == 1
                    && matches!(non_setline_body[0], Stmt::Expr(Expr::Call { name: fn_name, .. })
                        if fn_name.resolve() == "__mutsu_stub_die"
                            || fn_name.resolve() == "__mutsu_stub_warn");
                if *is_unit {
                    // unit module/package — set package for the rest of the scope
                    self.current_package = qualified_name.clone();
                    self.in_unit_package = true;
                    // A `grammar` body legitimately holds attributes; only
                    // `module`/`package` bodies reject `has`.
                    if !matches!(kind, crate::ast::PackageKind::Grammar) {
                        self.current_package_kind = Some(*kind);
                    }
                    // Register the package name so it's accessible as a value
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                    self.code.emit(OpCode::SetPackageKind {
                        name_idx,
                        kind: *kind,
                    });
                    // Keep the runtime package in step with the compiler's, so
                    // routines declared after this point register under
                    // `Foo::name` instead of leaking into `GLOBAL::` (PLAN 8.22).
                    self.code.emit(OpCode::SetCurrentPackage { name_idx });
                } else if is_stub_body {
                    // Stub package — register name but don't execute the body
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    self.code.emit(OpCode::RegisterPackage { name_idx });
                    self.code.emit(OpCode::SetPackageKind {
                        name_idx,
                        kind: *kind,
                    });
                    self.code.emit(OpCode::RegisterPackageStub { name_idx });
                } else {
                    let name_idx = self.code.add_constant(Value::str(qualified_name.clone()));
                    // Non-unit package declarations also produce a type object value.
                    if *is_my {
                        self.code.emit(OpCode::RegisterPackageMy { name_idx });
                    } else {
                        self.code.emit(OpCode::RegisterPackage { name_idx });
                    }
                    self.code.emit(OpCode::SetPackageKind {
                        name_idx,
                        kind: *kind,
                    });
                    // Clear any previous stub status for this package
                    self.code.emit(OpCode::ClearPackageStub { name_idx });
                    let pkg_idx = self.code.emit(OpCode::PackageScope {
                        name_idx,
                        body_end: 0,
                    });
                    let saved_package = self.current_package.clone();
                    let saved_in_unit = self.in_unit_package;
                    let saved_package_kind = self.current_package_kind;
                    self.current_package = qualified_name;
                    // Inside a non-unit `module Foo { ... }` block, runtime
                    // PackageScope handles the package context, so we must
                    // not pre-qualify nested class/role decls here.
                    self.in_unit_package = false;
                    // A `grammar` body legitimately holds attributes; only
                    // `module`/`package` bodies reject `has`.
                    self.current_package_kind = if matches!(kind, crate::ast::PackageKind::Grammar)
                    {
                        None
                    } else {
                        Some(*kind)
                    };
                    // Hoist `my sub` declarations so they are visible to earlier
                    // statements in the same package block (a `my sub` is lexically
                    // scoped and compile-time-visible throughout its block). Without
                    // this, a forward reference inside `package P { f(); my sub f {…} }`
                    // failed with "Unknown function". Sub bodies and inline blocks
                    // already hoist; the non-unit package body did not.
                    self.hoist_sub_decls(body, true);
                    self.hoist_type_decl_shells(body);
                    for s in body {
                        self.compile_stmt(s);
                    }
                    self.current_package = saved_package;
                    self.in_unit_package = saved_in_unit;
                    self.current_package_kind = saved_package_kind;
                    self.code.patch_body_end(pkg_idx);
                }
            }

            // ADR-0048 Phase 2: no phaser body takes a signature in raku
            // (`$^c` inside `BEGIN`/`ENTER`/`LEAVE`/`CONTROL`/`CATCH`/... is
            // `X::Placeholder::Block`). This covers every `Stmt::Phaser` kind
            // that reaches `compile_stmt` directly (BEGIN/CHECK/INIT/ENTER at
            // top level/END/PRE/POST, per the arms below); LEAVE/KEEP/UNDO/
            // FIRST/NEXT/LAST/CLOSE are extracted and compiled elsewhere
            // (`helpers_block_inline.rs`, `expand_loop_phasers`) before ever
            // reaching this match, so they are not covered by this guard —
            // left as a known gap for a follow-up.
            Stmt::Phaser { body, .. } if self.emit_block_placeholder_die(body) => {}
            // --- Phaser (BEGIN/CHECK/INIT) ---
            // These are extracted before compilation by extract_check_init_phasers()
            // and run in the correct order. If one remains (e.g. inside a sub body),
            // compile it inline as a fallback.
            Stmt::Phaser {
                kind: PhaserKind::Check,
                body,
            } => {
                // CHECK phasers run at compile time. If an error occurs inside
                // a CHECK phaser, Raku wraps it in X::Comp::BeginTime.
                self.compile_check_phaser(body);
            }
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
            } => {
                // BEGIN runs at compile time; an error thrown inside it is wrapped
                // in X::Comp::BeginTime (same mechanism as CHECK — the
                // CheckPhaserStart/End opcodes raise the `check_phaser_depth`
                // counter, and a throw at depth > 0 is wrapped). The opcodes don't
                // touch the stack, so the body's value/side-effects are preserved.
                self.compile_check_phaser(body);
            }
            Stmt::Phaser {
                kind: PhaserKind::Init | PhaserKind::Enter,
                body,
            } => {
                // INIT runs at run start, ENTER on block entry — neither is a
                // compile-time phaser, so their errors are NOT X::Comp::BeginTime.
                // ENTER at top-level scope compiles inline (in sub/method/closure
                // bodies it is handled by BlockScope and filtered out before
                // reaching this match arm).
                for s in body {
                    self.compile_stmt(s);
                }
            }
            Stmt::Phaser {
                kind: PhaserKind::End,
                body,
            } => {
                // END: store body in stmt pool for deferred execution
                let end_stmt = Stmt::Phaser {
                    kind: PhaserKind::End,
                    body: body.clone(),
                };
                let idx = self.code.add_stmt(end_stmt);
                let site_id =
                    super::STATE_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed) as u64;
                self.code.emit(OpCode::PhaserEnd { idx, site_id });
            }
            Stmt::Phaser {
                kind: PhaserKind::Pre,
                body,
            } => {
                // PRE phaser inline: compile body, check truthiness
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
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
                let condition_idx = self.phaser_condition_idx(body);
                self.code.emit(OpCode::CheckPhaser {
                    is_pre: true,
                    condition_idx,
                });
            }
            Stmt::Phaser {
                kind: PhaserKind::Post,
                body,
            } => {
                // POST phaser inline: compile body, check truthiness
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
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
                let condition_idx = self.phaser_condition_idx(body);
                self.code.emit(OpCode::CheckPhaser {
                    is_pre: false,
                    condition_idx,
                });
            }
            Stmt::Phaser { .. } => {}

            // --- SubDecl: delegate to interpreter AND compile body ---
            Stmt::SubDecl {
                name,
                name_expr,
                params,
                param_defs,
                return_type,
                signature_alternates,
                body,
                multi,
                is_rw,
                is_raw,
                custom_traits,
                ..
            } => {
                // A user-defined operator overrides even native `Int + Int`, so
                // it disables constant folding for the whole unit (ADR-0006
                // §2.1). A runtime-named sub (`sub ::($n)`) could be anything.
                self.note_operator_decl(&name.resolve());
                if name_expr.is_some() {
                    self.fold_ctx.note_operator_decl();
                }
                // Reject overriding a reserved special-form operator
                // (`infix:<=>`, `infix:<:=>`, `infix:<::=>`, `infix:<~~>`,
                // `prefix:<|>`) — these are handled directly by the compiler and
                // cannot be user-defined (X::Syntax::Extension::SpecialForm).
                if let Some(err_val) = Self::check_special_form_override(&name.resolve()) {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // Validate placeholder conflicts for subs with implicit params
                if param_defs.is_empty()
                    && !params.is_empty()
                    && let Some(err_val) =
                        self.check_placeholder_conflicts(params, body, Some("sub"))
                {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // Compile-time check: assignment to native-typed read-only
                // params (e.g. `sub foo(int $x) { $x = 42 }`) is an error.
                if let Some(err_val) =
                    Self::check_native_readonly_param_assignment(param_defs, body)
                {
                    let idx = self.code.add_constant(err_val);
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Die);
                    return;
                }
                // The hoist pass marks its copy of a body-local declaration
                // `__lexical_hoist`; the in-sequence registration is the same
                // declaration and has to say so too, or registering it a second
                // time reports the *sibling* scope's routine of that name as a
                // redeclaration.
                let body_local =
                    self.in_lexical_scope && !self.lexical_dup_routines.contains(&name.resolve());
                let idx =
                    if body_local && !custom_traits.iter().any(|(t, _)| t == "__lexical_hoist") {
                        let mut marked = stmt.clone();
                        if let Stmt::SubDecl { custom_traits, .. } = &mut marked {
                            custom_traits.push(("__lexical_hoist".to_string(), None));
                        }
                        self.add_sub_decl_plan(&marked)
                    } else {
                        self.add_sub_decl_plan(stmt)
                    };
                self.code.emit(OpCode::RegisterDecl(idx));
                // Also compile the body to bytecode for VM-native dispatch. This
                // runs even for a runtime-resolved name (`sub ::($n) {...}`): the
                // compiled_fns key below is an internal lookup symbol keyed off
                // the parsed placeholder text plus package/arity/fingerprint, not
                // the eventual runtime name, so it stays reliable — the *routine*
                // registers under `resolved_name` at RegisterDecl time regardless
                // of what key its bytecode was filed under (ADR-0019 C6e-3c).
                let state_group = if *multi && !signature_alternates.is_empty() {
                    Some(format!(
                        "{}::{}",
                        name,
                        crate::ast::function_body_fingerprint(params, param_defs, body)
                    ))
                } else {
                    None
                };
                let name_str = name.resolve();
                // Extract deprecation info from custom_traits
                let deprecated_info = custom_traits.iter().find_map(|(t, _)| {
                    if t == "DEPRECATED" {
                        Some((
                            "Sub".to_string(),
                            name_str.clone(),
                            self.current_package.clone(),
                            String::new(),
                        ))
                    } else {
                        t.strip_prefix("DEPRECATED:").map(|msg| {
                            (
                                "Sub".to_string(),
                                name_str.clone(),
                                self.current_package.clone(),
                                msg.to_string(),
                            )
                        })
                    }
                });
                // An `our sub` outlives its declaring block via the package
                // registry; flag the body compile so its read/write lexical
                // captures are boxed + persisted (escaping_our_sub_captures).
                let prev_our = self.compiling_our_sub;
                self.compiling_our_sub = custom_traits.iter().any(|(t, _)| t == "__our_scoped");
                let mut compiled_routine_keys = Vec::new();
                let is_cached = custom_traits.iter().any(|(t, _)| t == "cached");
                if let Some(key) = self.compile_sub_body_with_deprecation(
                    &name_str,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    body,
                    *multi,
                    state_group.as_deref(),
                    *is_rw,
                    *is_raw,
                    is_cached,
                    deprecated_info.clone(),
                ) {
                    compiled_routine_keys.push(key);
                }
                self.compiling_our_sub = prev_our;
                for (alt_params, alt_param_defs) in signature_alternates {
                    if let Some(key) = self.compile_sub_body_with_deprecation(
                        &name_str,
                        alt_params,
                        alt_param_defs,
                        return_type.as_ref(),
                        body,
                        *multi,
                        state_group.as_deref(),
                        *is_rw,
                        *is_raw,
                        is_cached,
                        deprecated_info.clone(),
                    ) {
                        compiled_routine_keys.push(key);
                    }
                }
                // The hoist pass registered this same declaration from a plan of
                // its own, which never sees the compiled bodies. Hand them over,
                // so a `multi` candidate installed by the hoisted registration
                // carries the plan's bytecode too.
                if let Some(fp) = self.code.sub_decl_plan_fingerprint(idx)
                    && let Some(pos) = self
                        .hoisted_sub_plans
                        .iter()
                        .position(|(n, f, _)| *n == *name && *f == fp)
                {
                    let (_, _, hoisted_idx) = self.hoisted_sub_plans.remove(pos);
                    self.code.set_sub_decl_compiled_routine_keys(
                        hoisted_idx,
                        compiled_routine_keys.clone(),
                    );
                }
                self.code
                    .set_sub_decl_compiled_routine_keys(idx, compiled_routine_keys);
            }
            Stmt::MethodDecl {
                name,
                name_expr,
                params,
                param_defs,
                body,
                multi,
                is_rw,
                return_type,
                ..
            } => {
                // Top-level/package method declarations should still produce callable
                // code objects (&name), so lower them through sub registration.
                let lowered = Stmt::SubDecl {
                    name: *name,
                    name_expr: name_expr.clone(),
                    params: params.clone(),
                    param_defs: param_defs.clone(),
                    return_type: return_type.clone(),
                    associativity: None,
                    precedence_trait: None,
                    signature_alternates: Vec::new(),
                    body: body.clone(),
                    multi: *multi,
                    is_rw: *is_rw,
                    is_raw: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    is_test_assertion: false,
                    supersede: false,
                    custom_traits: vec![("__mutsu_method_decl".to_string(), None)],
                };
                let idx = self.add_sub_decl_plan(&lowered);
                self.code.emit(OpCode::RegisterDecl(idx));
                if name_expr.is_none() {
                    let mut method_params: Vec<String> = vec![
                        "self".to_string(),
                        "__ANON_STATE__".to_string(),
                        "?CLASS".to_string(),
                        "?ROLE".to_string(),
                    ];
                    method_params.extend(params.iter().cloned());
                    let compiled_routine_keys = self
                        .compile_sub_body(
                            &name.resolve(),
                            &method_params,
                            param_defs,
                            return_type.as_ref(),
                            body,
                            *multi,
                            None,
                            *is_rw,
                            false,
                            false,
                        )
                        .into_iter()
                        .collect();
                    self.code
                        .set_sub_decl_compiled_routine_keys(idx, compiled_routine_keys);
                }
            }
            Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => {
                let idx = self.code.add_token_decl_plan(stmt);
                self.code.emit(OpCode::RegisterDecl(idx));
            }
            Stmt::ProtoDecl {
                name,
                params,
                param_defs,
                return_type,
                body,
                is_method,
                ..
            } => {
                self.note_operator_decl(&name.resolve());
                let idx = self.code.add_proto_decl_plan(stmt);
                // A trivial proto body (empty, or a bare `{*}`) dispatches
                // implicitly and has no candidate body of its own to compile
                // (mirrors `vm_resolve_trivial_proto_candidate`'s gate). A
                // `proto method`/`proto submethod` never installs at the
                // package level (Phase D territory) and compiles no body here
                // either — see `CompiledProtoDeclPlan::is_method`.
                let significant: Vec<&Stmt> = body
                    .iter()
                    .filter(|s| !matches!(s, Stmt::SetLine(_)))
                    .collect();
                let trivial = significant.is_empty()
                    || (significant.len() == 1
                        && matches!(significant[0], Stmt::Expr(Expr::Whatever)));
                if !*is_method && !trivial {
                    let rewritten = crate::runtime::Interpreter::rewrite_proto_dispatch_stmts(body);
                    let compiled_routine_key = self.compile_sub_body(
                        &name.resolve(),
                        params,
                        param_defs,
                        return_type.as_ref(),
                        &rewritten,
                        false,
                        None,
                        false,
                        false,
                        false,
                    );
                    self.code
                        .set_proto_decl_compiled_routine_key(idx, compiled_routine_key);
                }
                self.code.emit(OpCode::RegisterDecl(idx));
            }
            Stmt::ProtoToken { name } => {
                let idx = self.code.add_proto_token_decl_plan(*name);
                self.code.emit(OpCode::RegisterDecl(idx));
            }
            Stmt::Use { module, arg, .. } if module == "lib" && arg.is_some() => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    self.code.emit(OpCode::UseLibPath);
                }
            }
            Stmt::Use { module, arg, .. } if module == "lib" && arg.is_none() => {}
            Stmt::Use { module, arg, .. } if module == "dynamic-scope" => {
                self.apply_dynamic_scope_pragma(arg.as_ref());
            }
            Stmt::Use { module, arg, .. } if module == "newline" => {
                if let Some(expr) = arg {
                    self.compile_expr(expr);
                    let name_idx = self
                        .code
                        .add_constant(Value::str_from("__mutsu_set_newline"));
                    self.code.emit(OpCode::ExecCall {
                        name_idx,
                        arity: 1,
                        arg_sources_idx: None,
                    });
                }
            }
            Stmt::Use { module, arg, .. } if module == "variables" => {
                // `use variables :D/:U/:_` pragma — emit a SetVariablesPragma opcode
                if let Some(arg_expr) = arg {
                    self.compile_expr(arg_expr);
                } else {
                    let nil_idx = self.code.add_constant(Value::NIL);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                }
                let name_idx = self.code.add_constant(Value::str("variables".to_string()));
                self.code.emit(OpCode::SetPragma(name_idx));
            }
            Stmt::Use { module, arg, .. } if module == "attributes" => {
                // `use attributes :D/:U/:_` pragma — emit a SetPragma opcode
                if let Some(arg_expr) = arg {
                    self.compile_expr(arg_expr);
                } else {
                    let nil_idx = self.code.add_constant(Value::NIL);
                    self.code.emit(OpCode::LoadConst(nil_idx));
                }
                let name_idx = self.code.add_constant(Value::str("attributes".to_string()));
                self.code.emit(OpCode::SetPragma(name_idx));
            }
            Stmt::Use { module, .. }
                if module == "v6"
                    || module == "customtrait"
                    || module == "isms"
                    || module == "nqp"
                    || module == "soft"
                    || module == "oo"
                    || module == "class"
                    // `use experimental :pack/:cached/:macros/...` enables
                    // experimental features that mutsu provides unconditionally
                    // (e.g. pack/unpack), so the pragma is a compile-time no-op.
                    || module == "experimental" => {}
            Stmt::Use { module, .. } if module == "MONKEY-TYPING" || module == "MONKEY" => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::UseModule {
                    name_idx,
                    tags_idx: None,
                    arg_count: 0,
                });
            }
            Stmt::Use { module, arg, .. } if module == "Test::More" => {
                self.compile_test_more_use(arg);
            }
            Stmt::Use { module, tags, .. } if module == "Test" || module.starts_with("Test::") => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                let tags_idx = if tags.is_empty() {
                    None
                } else {
                    let entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                    Some(self.code.add_constant(Value::array(entries)))
                };
                self.code.emit(OpCode::UseModule {
                    name_idx,
                    tags_idx,
                    arg_count: 0,
                });
            }
            // `use if;` — the bare `if` pragma module itself is a no-op; it only
            // provides the `:if(...)` adverb handled below.
            Stmt::Use {
                module,
                condition: None,
                ..
            } if module == "if" => {}
            Stmt::Use {
                module,
                tags,
                condition,
                arg,
            } => {
                // A module `use`d here may export operators
                // (`multi infix:<...> is export`). Because mutsu loads modules
                // at *runtime*, the compiler cannot see those exports while
                // compiling the consuming unit, so a literal-only expression
                // like `64 ** ⅓` after `use Rat::Power` would otherwise be
                // folded against the *core* operator before the module's
                // override is installed (ADR-0006 known gap). Treat a real
                // module import like an inline operator declaration: if the
                // unit folded any literal operator expression, the unit-level
                // compile recompiles it with folding off. Only units that both
                // import a module and fold a literal operator pay this (the
                // `folded` flag is set solely by operator literal folds), so
                // constant inlining in the common case is unaffected. Pragmas
                // (`v6`, `strict`, `nqp`, ...) are matched by earlier arms and
                // never reach here, so they keep folding.
                self.fold_ctx.note_operator_decl();
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                // The native JSON modules read their import list at run time to
                // select per-scope defaults (`use JSON::Fast <immutable !pretty>`).
                // The angle-list words parse into `arg`, not `tags`; ride them in
                // the same tags constant (unused otherwise for native modules).
                let mut entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                if matches!(module.as_str(), "JSON::Fast" | "JSON::Tiny")
                    && let Some(arg) = arg
                {
                    let words: &[Expr] = match arg {
                        Expr::ArrayLiteral(items) => items,
                        other => std::slice::from_ref(other),
                    };
                    for w in words {
                        if let Expr::Literal(lit) = w
                            && let ValueView::Str(s) = lit.view()
                        {
                            entries.push(Value::str(s.to_string()));
                        }
                    }
                }
                let tags_idx = if entries.is_empty() {
                    None
                } else {
                    Some(self.code.add_constant(Value::array(entries)))
                };
                // `use`-arguments (`use Foo "a", "b"` / `use Foo <a b c>`) are
                // evaluated here and pushed on the stack for the module's
                // `sub EXPORT`. A `<a b c>` word list flattens into positional
                // args, matching `sub EXPORT(*@args) { ... }` seeing three items.
                let arg_exprs: Vec<&Expr> = match arg {
                    Some(Expr::ArrayLiteral(items)) => items.iter().collect(),
                    Some(other) => vec![other],
                    None => vec![],
                };
                let arg_count = arg_exprs.len() as u16;
                // `use Foo:if(EXPR)` (the `if` pragma): load the module only when
                // EXPR is true at runtime, evaluated here so platform-conditional
                // imports (`use Foo:if($*DISTRO.is-win)`) pick the right branch.
                if let Some(cond) = condition {
                    self.compile_expr(cond);
                    let skip = self.code.emit(OpCode::JumpIfFalse(0));
                    for e in &arg_exprs {
                        self.compile_expr(e);
                    }
                    self.code.emit(OpCode::UseModule {
                        name_idx,
                        tags_idx,
                        arg_count,
                    });
                    self.code.patch_jump(skip);
                } else {
                    for e in &arg_exprs {
                        self.compile_expr(e);
                    }
                    self.code.emit(OpCode::UseModule {
                        name_idx,
                        tags_idx,
                        arg_count,
                    });
                }
            }
            Stmt::Import { module, tags } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                let tags_idx = if tags.is_empty() {
                    None
                } else {
                    let entries = tags.iter().cloned().map(Value::str).collect::<Vec<Value>>();
                    Some(self.code.add_constant(Value::array(entries)))
                };
                self.code.emit(OpCode::ImportModule { name_idx, tags_idx });
            }
            Stmt::No { module, .. } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::NoModule(name_idx));
            }
            Stmt::Need { module } => {
                let name_idx = self.code.add_constant(Value::str(module.clone()));
                self.code.emit(OpCode::NeedModule(name_idx));
            }
            Stmt::EnumDecl {
                name,
                variants,
                is_my,
                ..
            } => {
                // A `my enum` binds its type name AND every variant name lexically
                // in this block, exactly like a `my` variable — so record them as
                // the block's own declarations. That is what makes them survive
                // into a `whenever` callback of the enclosing `supply { … }` body
                // (they are installed with overwrite as `authoritative_captures`)
                // instead of losing to a same-named outer binding when the callback
                // runs from the emitting thread's env: `supply { my enum E
                // <StatusLine Header Body>; whenever … { … Header … } }` resolved
                // `Header` to a file-scope `class …::Header` alias. It also stops
                // the binding leaking back to the caller on block exit, which is
                // what the same set already does for `my $x`.
                if *is_my {
                    self.code.my_declared_sym.insert(*name);
                    self.code.my_declared_enum_sym.insert(*name);
                    for (variant, _) in variants {
                        let sym = Symbol::intern(variant);
                        self.code.my_declared_sym.insert(sym);
                        self.code.my_declared_enum_sym.insert(sym);
                    }
                }
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterEnum(idx));
            }
            Stmt::ClassDecl { body, .. } if self.emit_block_placeholder_die(body) => {}
            Stmt::ClassDecl { name, body, .. } => {
                // Declaring the same class name twice in one lexical scope is an
                // X::Redeclaration ("Redeclaration of symbol 'A'"), matching Raku's
                // compile-time check. A stub (`class A {...}`) followed by its real
                // definition is NOT a redeclaration (the stub carries no full body),
                // and a same-named class in an inner block shadows rather than
                // redeclares (the current-scope set is reset on block entry).
                if !Self::is_stub_class_body(body) {
                    let cname = name.resolve();
                    // Package blocks share this compiler scope, so key the
                    // declaration by the same package-qualified name that
                    // RegisterClass uses. Otherwise `module A1 { class N::C {} }`
                    // and `module A2 { class N::C {} }` both occupy the bare
                    // `N::C` key and the second declaration is rejected.
                    let redeclaration_key = if let Some(absolute) = cname.strip_prefix("GLOBAL::") {
                        absolute.to_string()
                    } else if self.current_package == "GLOBAL"
                        || cname == self.current_package
                        || cname.starts_with(&format!("{}::", self.current_package))
                    {
                        cname.clone()
                    } else {
                        format!("{}::{}", self.current_package, cname)
                    };
                    if !self.class_names_current_scope.insert(redeclaration_key) {
                        let sym = cname.rsplit("::").next().unwrap_or(&cname).to_string();
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(sym));
                        attrs.insert("what".to_string(), Value::str_from("symbol"));
                        let err = Value::make_instance(Symbol::intern("X::Redeclaration"), attrs);
                        let cidx = self.code.add_constant(err);
                        self.code.emit(OpCode::LoadConst(cidx));
                        self.code.emit(OpCode::Die);
                        return;
                    }
                }
                // A method installed by RegisterClass outlives this frame and has
                // no closure-creation op, so a frame lexical it writes must keep
                // the name-keyed shared lane. That set is harvested inside
                // `add_class_decl_plan`'s own method-body compile pass (see
                // `record_type_body_written_lexicals`), not by a separate
                // analysis compile here.
                // Pre-qualify the class name when compiling inside a
                // `unit module`/`unit class` body so that the runtime
                // registers it under the correct nested package
                // (e.g. `class D` inside `unit module A::B` → `A::B::D`).
                let stmt = self.qualify_decl_name(stmt);
                let idx = self.add_class_decl_plan(&stmt);
                self.code.emit(OpCode::RegisterDecl(idx));
            }
            Stmt::AugmentClass { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::AugmentClass(idx));
            }
            Stmt::RoleDecl { body, .. } if self.emit_block_placeholder_die(body) => {}
            Stmt::RoleDecl { .. } => {
                // Same as RegisterClass above: a role method has no creation op,
                // and `add_role_decl_plan`'s method-body compile pass harvests
                // the lexicals its methods write.
                let stmt = self.qualify_decl_name(stmt);
                let idx = self.add_role_decl_plan(&stmt);
                self.code.emit(OpCode::RegisterDecl(idx));
            }
            Stmt::SubsetDecl { .. } => {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSubset(idx));
            }
            Stmt::Subtest { name, body } => {
                self.compile_expr(name);
                let idx = self.code.emit(OpCode::SubtestScope { body_end: 0 });
                for s in body {
                    self.compile_stmt(s);
                }
                self.code.patch_body_end(idx);
            }
            Stmt::Whenever {
                supply,
                param,
                param_type,
                body,
            } => {
                self.compile_expr(supply);
                let body_idx = self.code.add_stmt(Stmt::Block(body.clone()));
                // A whenever block without a pointy param may still declare
                // its parameter as a placeholder (`whenever $ch { %^content.kv
                // }`): the emitted value binds to it, arity-1 like `-> $v`.
                let param = param.clone().or_else(|| {
                    crate::ast::collect_placeholders_shallow(body)
                        .into_iter()
                        .next()
                });
                // Case B (cross-thread lexicals): surface the runtime-compiled
                // body's free vars so a captured-and-mutated lexical read
                // directly in the whenever body (`start { react { whenever $ch
                // { ...read $gate... } } }`) is cell-promoted and sees the
                // parent's post-registration writes. See
                // surface_stashed_body_free_vars for the mechanism.
                let analysis_param = vec![param.clone().unwrap_or_else(|| "$_".to_string())];
                // LAST/QUIT callbacks are split out of the whenever body at
                // runtime. Compile their statements inline only in the
                // analysis copy so their outer lexical reads contribute to the
                // parent-slot inventory; the executable stmt_pool body retains
                // the original Phaser nodes.
                let mut analysis_body = Vec::new();
                for stmt in body {
                    if let Stmt::Phaser {
                        kind: PhaserKind::Last | PhaserKind::Quit,
                        body: phaser_body,
                    } = stmt
                    {
                        analysis_body.extend(phaser_body.iter().cloned());
                    } else {
                        analysis_body.push(stmt.clone());
                    }
                }
                let analysis_cc_idx =
                    self.surface_stashed_body_free_vars(&analysis_param, &analysis_body);
                let param_idx = param
                    .as_ref()
                    .map(|p| self.code.add_constant(Value::str(p.clone())));
                let param_type_idx = param_type
                    .as_ref()
                    .map(|t| self.code.add_constant(Value::str(t.clone())));
                // Only bridge the tap handle out through `env[$s]` when this
                // whenever is the value of a `do whenever $s {...}` expression
                // (`whenever_bind_target`). A bare `whenever $s {...}` statement
                // must NOT clobber `$s` with its Tap — otherwise re-tapping the
                // same supply on a later iteration (a nested `whenever` inside
                // `whenever Supply.interval(...)`) would see a Tap, not the Supply.
                let target_var_idx = if self.whenever_bind_target
                    && let Expr::Var(name) = supply
                {
                    Some(self.code.add_constant(Value::str(name.clone())))
                } else {
                    None
                };
                self.code.emit(OpCode::WheneverScope {
                    body_idx,
                    analysis_cc_idx,
                    param_idx,
                    target_var_idx,
                    param_type_idx,
                });
            }
            Stmt::Let {
                name,
                index,
                value,
                is_temp,
                undefine_first,
            } => {
                // Temporizing a never-declared dynamic variable (`temp $*foo`)
                // throws X::Dynamic::NotFound — you can only `temp`/`let` a variable
                // that is already in scope. Emit the guard before the save (a no-op
                // for non-dynamic names and for an already-declared dynamic). Skip
                // it for the element form (`temp $*arr[0]`), which temporizes a
                // container element rather than the dynamic itself.
                if index.is_none() {
                    self.maybe_emit_dynamic_var_check(name);
                }
                // If undefine_first is set, assign Nil to the variable before saving.
                // This makes LetSave capture the undefined state, so on scope exit
                // the variable is restored to undefined (and its default value applies).
                if *undefine_first {
                    self.compile_expr(&Expr::Literal(Value::NIL));
                    self.emit_set_named_var(name);
                }
                // Emit LetSave: saves current value of the variable
                let name_idx = self.code.add_constant(Value::str(name.clone()));
                let has_index = index.is_some();
                // Bake the scalar's slot for the scope-exit restore (§1.4/§1.5).
                // Index mode (`temp @a[$i]`) restores a container ELEMENT, not the
                // named variable's slot, so keep the by-name path there.
                let slot = if has_index {
                    None
                } else {
                    self.local_map.get(name).copied()
                };
                if let Some(idx_expr) = index {
                    self.compile_expr(idx_expr);
                }
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: has_index,
                    is_temp: *is_temp,
                    slot,
                });
                // Compile the assignment if value is provided
                if let Some(val_expr) = value {
                    if has_index {
                        // For array/hash index assignment: compile as Stmt::Expr(IndexAssign)
                        let is_hash = name.starts_with('%');
                        let target_expr = if let Some(stripped) = name.strip_prefix('@') {
                            Expr::ArrayVar(stripped.to_string())
                        } else if let Some(stripped) = name.strip_prefix('%') {
                            Expr::HashVar(stripped.to_string())
                        } else {
                            Expr::Var(name.to_string())
                        };
                        let assign_expr = Expr::IndexAssign {
                            target: Box::new(target_expr),
                            index: Box::new(index.as_ref().unwrap().as_ref().clone()),
                            value: Box::new(val_expr.as_ref().clone()),
                            is_positional: !is_hash,
                        };
                        self.compile_expr(&assign_expr);
                        self.code.emit(OpCode::Pop);
                    } else {
                        self.compile_expr(val_expr);
                        self.emit_set_named_var(name);
                    }
                }
            }
            Stmt::TempMethodAssign {
                var_name,
                method_name,
                method_args,
                value,
            } => {
                let slot = self.local_map.get(var_name).copied();
                let name_idx = self.code.add_constant(Value::str(var_name.clone()));
                self.code.emit(OpCode::LetSave {
                    name_idx,
                    index_mode: false,
                    is_temp: true,
                    slot,
                });
                let assign_expr = Expr::Call {
                    name: Symbol::intern("__mutsu_assign_method_lvalue"),
                    args: vec![
                        Expr::Var(var_name.clone()),
                        Expr::Literal(Value::str(method_name.clone())),
                        Expr::ArrayLiteral(method_args.clone()),
                        value.clone(),
                        Expr::Literal(Value::str(var_name.clone())),
                    ],
                };
                self.compile_expr(&assign_expr);
                self.code.emit(OpCode::Pop);
            }
            Stmt::SetLine(line) => {
                // No instruction: the line is static data. Attach it to every op
                // emitted for the statements that follow (`CompiledCode::op_lines`),
                // and let the VM read it back from `ip` where a line is observable.
                self.last_source_line = Some(*line);
                self.code.set_emit_line(*line);
            }
        }
    }

    /// Compile the last statement of a `let` block so its result sets the topic.
    /// This allows `exec_let_block_op` to check the topic for success/failure.
    pub(super) fn compile_last_stmt_as_topic(&mut self, stmt: &Stmt) {
        self.compile_tail_stmt_value(stmt);
        self.code.emit(OpCode::SetTopic);
    }

    /// Like [`compile_last_stmt_as_topic`], but leaves the statement's value on
    /// the value stack instead of routing it through the topic. Used for the
    /// final statement of a block compiled in expression context (e.g. a `do`
    /// block), whose value the enclosing `DoBlockExpr` pops off the stack.
    pub(super) fn compile_last_stmt_as_value(&mut self, stmt: &Stmt) {
        self.compile_tail_stmt_value(stmt);
    }

    /// Compile a block-final statement so exactly one value — the statement's
    /// Raku value — is left on the stack. Mirrors the tail-statement arms of
    /// `Compiler::compile`: a tail `if`/`with` yields its branch value (a
    /// module-loaded `sub { ...; LEAVE {...}; with ptr {...} else {...} }` — the
    /// NativeHelpers::Blob shape — used to yield `True` here), a declaration or
    /// assignment yields the assigned value, and only genuinely valueless
    /// statements fall back to `True` ("completed" success).
    fn compile_tail_stmt_value(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                // Tail expression escapes the frame (implicit result).
                self.with_escape(true, |c| c.compile_expr(expr));
            }
            Stmt::Call { name, args } => {
                self.compile_tail_stmt_call_value(*name, args);
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
            }
            // A statement `given` nets exactly one stack value (see
            // `exec_given_op`), which IS the block value here.
            Stmt::Given { .. } => self.compile_stmt(stmt),
            Stmt::Block(body) => {
                self.compile_block_inline(body);
            }
            Stmt::SyntheticBlock(body) => {
                // A parser wrapper (e.g. a tail `my $*x := ...` bind used as
                // the last statement of a phaser-carrying block or a `let`
                // block), not a real lexical scope -- see
                // `compile_synthetic_block_inline`. The callers here
                // (`compile_phaser_block_scope`, the `let`-block path in
                // `Stmt::Block`'s own compile arm) do not push their own
                // dynamic-var scope, so this statement shares the enclosing
                // block's scope and must stay transparent to it.
                self.compile_synthetic_block_inline(body);
            }
            Stmt::VarDecl { name, .. } => {
                let var_name = name.clone();
                self.compile_stmt(stmt);
                let slot = self.alloc_local(&var_name);
                self.code.emit(OpCode::GetLocal(slot));
            }
            Stmt::Assign { name, .. } => {
                self.compile_stmt(stmt);
                if let Some(&slot) = self.local_map.get(name) {
                    self.code.emit(OpCode::GetLocal(slot));
                } else {
                    let idx = self
                        .code
                        .add_constant(Value::str(self.qualify_variable_name(name)));
                    self.code.emit(OpCode::GetGlobal(idx));
                }
            }
            _ => {
                // For genuinely valueless statements (loops, say, etc.),
                // compile normally. Any completed statement counts as success.
                self.compile_stmt(stmt);
                self.compile_expr(&Expr::Literal(Value::TRUE));
            }
        }
    }

    /// Compile PRE phasers in forward source order.
    /// Add the source text of a PRE/POST phaser's condition as a constant and
    /// return its index, for the X::Phaser::PrePost `condition`/message. The
    /// condition is the phaser body's final expression (e.g. `0`); block-form
    /// bodies and non-trivial expressions yield `None`.
    fn phaser_condition_idx(&mut self, body: &[Stmt]) -> Option<u32> {
        let last = body.last()?;
        let Stmt::Expr(expr) = last else { return None };
        let src = Self::deparse_phaser_condition(expr)?;
        Some(self.code.add_constant(Value::str(src)))
    }

    /// Best-effort source reconstruction of a phaser condition expression,
    /// covering the common literal/variable forms (e.g. statement-form
    /// `PRE 0`). Non-trivial expressions yield `None`.
    fn deparse_phaser_condition(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Literal(v) => Some(v.to_string_value()),
            Expr::Var(name) => Some(format!("${}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::BareWord(name) => Some(name.to_string()),
            _ => None,
        }
    }

    /// Each PRE body is compiled, followed by a CheckPhaser { is_pre: true }.
    pub(super) fn compile_pre_phasers(compiler: &mut Compiler, stmts: &[Stmt]) {
        for s in stmts {
            if let Stmt::Phaser {
                kind: PhaserKind::Pre,
                body,
            } = s
            {
                // ADR-0048 Phase 2: `PRE {}` does not take a signature in
                // raku. This helper is the only place a PRE body is compiled
                // (extracted from the enclosing block's statement list by
                // `compile_phaser_block_scope` before `compile_stmt` ever
                // sees the wrapping `Stmt::Phaser`), so the check lives here.
                if compiler.emit_block_placeholder_die(body) {
                    continue;
                }
                // Compile the PRE body as a block expression that produces a value
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        // Last statement: compile as expression to leave value on stack
                        match inner {
                            Stmt::Expr(expr) => compiler.compile_expr(expr),
                            _ => {
                                compiler.compile_stmt(inner);
                                compiler.compile_expr(&Expr::Literal(Value::TRUE));
                            }
                        }
                    } else {
                        compiler.compile_stmt(inner);
                    }
                }
                let condition_idx = compiler.phaser_condition_idx(body);
                compiler.code.emit(OpCode::CheckPhaser {
                    is_pre: true,
                    condition_idx,
                });
            }
        }
    }

    /// Compile POST phasers in reverse source order.
    /// Each POST body is compiled, followed by a CheckPhaser { is_pre: false }.
    pub(super) fn compile_post_phasers(compiler: &mut Compiler, stmts: &[Stmt]) {
        for s in stmts.iter().rev() {
            if let Stmt::Phaser {
                kind: PhaserKind::Post,
                body,
            } = s
            {
                // ADR-0048 Phase 2: `POST {}` does not take a signature in
                // raku — same reasoning as `compile_pre_phasers` above.
                if compiler.emit_block_placeholder_die(body) {
                    continue;
                }
                for (i, inner) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        match inner {
                            Stmt::Expr(expr) => compiler.compile_expr(expr),
                            _ => {
                                compiler.compile_stmt(inner);
                                compiler.compile_expr(&Expr::Literal(Value::TRUE));
                            }
                        }
                    } else {
                        compiler.compile_stmt(inner);
                    }
                }
                let condition_idx = compiler.phaser_condition_idx(body);
                compiler.code.emit(OpCode::CheckPhaser {
                    is_pre: false,
                    condition_idx,
                });
            }
        }
    }
}
