use super::*;

impl Interpreter {
    /// `@a[i].push(...)` / `%h<k>.pop` invocant load (container identity
    /// §3.2, post-call writeback removal): read the element like a plain
    /// subscript would. With `autoviv` (push/append/unshift/prepend), a
    /// missing element (Nil / Any / Mu hole) is autovivified to a fresh empty
    /// Array THROUGH the normal index-assign machinery (type checks, shapes,
    /// hole filling, itemization) and re-read from the live variable so the
    /// value pushed on the stack IS the stored shared node. Without `autoviv`
    /// (pop/shift/splice), a missing element is pushed as-is so the method
    /// dispatch raises the proper error without growing the container
    /// (matching Raku). The following method call then mutates the element's
    /// node in place and no post-call writeback is needed.
    ///
    /// Elements of a parameterized container (`my Array of Int @x`) get the
    /// inner element type tagged onto their node: the post-call writeback
    /// used to re-validate the whole element against the variable constraint,
    /// so without it the method's own metadata-based check
    /// (`check_array_value_element_types` / the splice replacement check)
    /// must be able to see the declared type.
    /// Stack: [container, key] → [element]
    pub(super) fn exec_index_elem_autoviv_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
        autoviv: bool,
    ) -> Result<(), RuntimeError> {
        let key = self.stack.pop().unwrap_or(Value::NIL);
        let container = self.stack.pop().unwrap_or(Value::NIL);
        // A Junction key (`%h{ any ^2 }.push: 42`) threads the whole
        // element-for-mutation load over its values, so every addressed
        // element is autovivified and the following method call autothreads
        // over a Junction of the stored shared nodes (the old post-call
        // writeback stored through the junction subscript instead).
        if let ValueView::Junction { kind, values } = key.view() {
            let mut elems = Vec::with_capacity(values.len());
            for v in values.iter() {
                self.stack.push(container.clone());
                self.stack.push(v.clone());
                self.exec_index_elem_autoviv_op(
                    code,
                    name_idx,
                    is_positional,
                    target_slot,
                    autoviv,
                )?;
                elems.push(self.stack.pop().unwrap_or(Value::NIL));
            }
            self.stack.push(Value::junction(kind.clone(), elems));
            return Ok(());
        }
        self.stack.push(container);
        self.stack.push(key.clone());
        self.exec_index_op_with_positional(is_positional)?;
        let mut elem = self.stack.pop().unwrap_or(Value::NIL);
        let name = Self::const_str(code, name_idx).to_string();
        let constraint = loan_env!(self, var_type_constraint(&name));
        // Raku's Any:U autovivification applies to any undefined (type
        // object) invocant whose type does not provide its own `push` —
        // including typed-container holes (`my Array of Int @y` reads a hole
        // as `(Array[Int])`) and scalar type objects (`(Int,)` → push
        // autovivifies `[]`, and the store's type check then dies for a
        // constrained element, exactly like Raku). Types with their own
        // `push` raise their own errors instead (List is immutable,
        // Hash.push wants pairs, ...).
        let missing = elem.is_nil()
            || matches!(elem.view(), ValueView::Package(p) if {
                !matches!(
                    p.resolve().split('[').next().unwrap_or(""),
                    "List" | "Seq" | "Slip" | "Range" | "Hash" | "Map" | "Buf" | "Blob"
                )
            });
        if missing && autoviv {
            // Store a fresh empty Array into the element (same machinery as
            // `@a[i] = []`), then re-read from the live variable: the store
            // may itemize (hash elements), fill holes, or create the
            // variable's container itself (`my $x; $x<k>.push(1)`), so the
            // held container value can be stale.
            self.stack.push(Value::real_array(Vec::new()));
            self.stack.push(key.clone());
            let pre = self.array_hash_attr_env_snapshot(code, name_idx);
            self.exec_index_assign_expr_named_op(code, name_idx, is_positional, target_slot)?;
            self.mirror_array_hash_attr_to_cell(code, name_idx, pre);
            let assigned = self.stack.pop().unwrap_or(Value::NIL);
            let mut container = self.get_env_with_main_alias(&name).unwrap_or(Value::NIL);
            if let ValueView::ContainerRef(cell) = container.view() {
                let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
                container = inner;
            }
            self.stack.push(container);
            self.stack.push(key);
            self.exec_index_op_with_positional(is_positional)?;
            elem = self.stack.pop().unwrap_or(Value::NIL);
            // Same-thread visibility gap: a shared-store element write inside
            // `start` drops the thread's private env copy, so the re-read can
            // miss the element that WAS stored in the shared canonical. The
            // assignment's result value is node-shared with what the shared
            // store inserted, so mutating it writes through to the canonical
            // (the parent observes it after the join).
            if self.shared_vars_active
                && (elem.is_nil() || matches!(elem.view(), ValueView::Package(_)))
            {
                elem = assigned.descalarize().clone();
            }
        }
        if let Some(constraint) = &constraint {
            Self::tag_element_value_type_in_place(&elem, constraint);
        }
        self.stack.push(elem);
        Ok(())
    }

    /// When a container variable's element type is itself a parameterized
    /// container (`my Array of Int @x` → constraint `Array[Int]`), embed the
    /// inner value type (`Int`) into the element's SHARED backing node so the
    /// in-place mutating methods' metadata checks enforce it. Only fills a
    /// node with no embedded value type yet; a declared type already on the
    /// node wins.
    fn tag_element_value_type_in_place(elem: &Value, constraint: &str) {
        let (inner, is_array) = if let Some(rest) = constraint.strip_prefix("Array[") {
            (rest.strip_suffix(']'), true)
        } else if let Some(rest) = constraint.strip_prefix("Hash[") {
            (rest.strip_suffix(']'), false)
        } else {
            return;
        };
        let Some(vt) = inner else { return };
        if vt.is_empty() || vt == "Any" || vt == "Mu" {
            return;
        }
        let mut holder = elem.clone();
        if is_array {
            holder.with_array_mut(|arc, _kind| {
                if arc.value_type.is_none() {
                    // SAFETY: audited aliased in-place container write (see
                    // value::aliased_mut); no other borrow into this node is
                    // live across the write below.
                    let data = unsafe { crate::value::gc_contents_mut(arc) };
                    data.value_type = Some(vt.to_string());
                    if data.declared_type.is_none() {
                        data.declared_type = Some(constraint.to_string());
                    }
                }
            });
        } else {
            holder.with_hash_mut(|arc| {
                if arc.value_type.is_none() {
                    // SAFETY: see above.
                    let data = unsafe { crate::value::gc_contents_mut(arc) };
                    data.value_type = Some(vt.to_string());
                    if data.declared_type.is_none() {
                        data.declared_type = Some(constraint.to_string());
                    }
                }
            });
        }
    }
}
