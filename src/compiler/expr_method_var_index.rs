use super::*;

impl Compiler {
    /// Is this subscript's index a *slice* rather than a single element?
    ///
    /// raku answers `.VAR` from the subscript's result: `@a[0]` hands back the
    /// element's `Scalar` container, while `@a[0,1]` hands back a `List` of
    /// containers, and `.VAR` on a `List` is identity — so `@a[0,1].VAR.^name`
    /// is `List`, not `Scalar`. A slice compiles as an ordinary subscript and
    /// lets `.VAR`'s normal dispatch (identity on a `List`) answer.
    ///
    /// Only the statically-recognizable slice spellings are covered:
    /// `@a[0,1]` / `%h<a b>` (an `ArrayLiteral` index — a single `<a>`
    /// collapses to a `Literal`, so it is not caught here), a range
    /// (`@a[0..1]`, `@a[^2]`), and an `@`-sigiled index (`@a[@idx]`). Every
    /// other slice spelling — `@a[*]`, and an index whose *runtime* value
    /// happens to be a list (`my $i = (0,1); @a[$i]`) — is caught instead by
    /// the value-side discriminator in `builtin_index_var_meta` (ADR-0064): a
    /// real container itemizes every element it stores, so a bare,
    /// non-itemized `List` arriving there never came out of one element slot.
    /// This gate stays because it is also the only one that works for a
    /// `LazyList`-backed container, where itemization is still incomplete.
    pub(super) fn index_expr_is_slice(index: &Expr) -> bool {
        match index {
            Expr::ArrayLiteral(_) | Expr::ArrayVar(_) => true,
            Expr::Binary { op, .. } => matches!(
                op,
                TokenKind::DotDot
                    | TokenKind::DotDotCaret
                    | TokenKind::CaretDotDot
                    | TokenKind::CaretDotDotCaret
            ),
            _ => false,
        }
    }

    /// The named container a `.VAR`-carrying subscript reads from, for both the
    /// one-dimensional (`@a[0]`, `%h<k>`) and multi-dimensional (`@sh[0;0]`)
    /// spellings. A shaped array's elements are `Scalar` containers exactly
    /// like a flat one's, so `@sh[0;0].VAR` takes the same element-descriptor
    /// path -- it just reaches the compiler as a different `Expr` node.
    ///
    /// Returns `None` for a statically-recognizable slice, which must compile
    /// as an ordinary subscript instead (see `index_expr_is_slice`).
    pub(super) fn var_on_index_source_name(&self, target: &Expr) -> Option<String> {
        let index_target = match target {
            Expr::Index {
                target: index_target,
                index,
                ..
            } => {
                if Self::index_expr_is_slice(index) {
                    return None;
                }
                index_target
            }
            Expr::MultiDimIndex {
                target: index_target,
                ..
            } => index_target,
            _ => return None,
        };
        self.index_assign_target_name(index_target)
    }

    /// Does this `.VAR` target take the element-descriptor path at all?
    ///
    /// A NAMED container's subscript always does (`var_on_index_source_name`).
    /// An un-named one does too -- a chained `%d<a><b>` / `@g[0][1]` (whose
    /// parent is an intermediate value with no variable name), a literal
    /// `[1,2][0]`, a `bump()[0]` -- because raku answers from the parent
    /// regardless of whether it has a name: `[1,2][0].VAR` is `Scalar` while
    /// `(1,2)[0].VAR` is `Int`.
    ///
    /// The two exclusions are the subscript shapes whose own compile path this
    /// helper's manual `parent; Dup; index; Index` emission would bypass: a
    /// `PseudoStash` subscript compiles to `GetCallerVar`/`GetOuterVar`, and
    /// `%*ENV<k>` to `GetEnvIndex`. Neither is a real container whose elements
    /// would answer `Scalar` anyway.
    pub(super) fn var_on_index_takes_element_path(&self, target: &Expr) -> bool {
        if self.var_on_index_source_name(target).is_some() {
            return true;
        }
        // A `:=` bind compiles subscripts through the autovivify opcodes; leave
        // that path alone entirely.
        if self.scalar_bind_autovivify {
            return false;
        }
        matches!(
            target,
            Expr::Index { target: it, index, .. }
                if !Self::index_expr_is_slice(index)
                    && !matches!(it.as_ref(), Expr::HashVar(_))
                    // A package stash (`Pkg::<$v>`) names its symbols by key,
                    // so it takes the element path; `CALLER::`, `OUTER::` ...
                    // compile to frame lookups instead.
                    && !matches!(
                        it.as_ref(),
                        Expr::PseudoStash(name)
                            if name.trim_end_matches("::").split("::").any(
                                crate::runtime::Interpreter::is_pseudo_package_name
                            )
                    )
        )
    }

    /// Compile a `.VAR` subscript's index, leaving it on the stack for the
    /// subscript read and keeping a copy in a hidden local, which the element
    /// descriptor reads to name a missing element after its subscript
    /// (`@b[5].VAR.name` is `@b[5]`, #19080). Returns the local's slot.
    pub(super) fn compile_var_meta_index(&mut self, index: &Expr) -> u32 {
        self.tmp_counter += 1;
        let slot = self.alloc_local(&format!("__mutsu_var_meta_index_{}", self.tmp_counter));
        self.compile_subscript_index(index);
        self.code.emit(OpCode::Dup);
        self.code.emit(OpCode::SetLocal(slot));
        slot
    }
}
