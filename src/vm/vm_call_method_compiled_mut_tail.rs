//! The tail of [`Interpreter::try_compiled_method_mut_or_interpret_sym`]: the
//! part that reaches an actual dispatch once every pre-dispatch probe in that
//! function's prefix has declined the call.
//!
//! Split out so the `CallMethodMut` plain-method lane (#8880, see
//! `vm_call_method_plain_lane`) can enter it directly for a
//! `(class, method)` pair whose prefix has already been proven inert, without
//! the prefix being duplicated or the shared code drifting between the two
//! entries.

use super::*;

impl Interpreter {
    /// Resolve the receiver's user method and dispatch it; failing that, the
    /// "lever A" native forks and finally the interpreter fallback.
    pub(super) fn compiled_mut_resolved_dispatch(
        &mut self,
        target_name: &str,
        target: Value,
        method_sym: crate::symbol::Symbol,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let method: &str = method_sym.as_str();
        // Reuse the receiver's already-interned class Symbol instead of
        // resolving it to a String and re-interning that String back to a
        // Symbol (`Symbol -> resolve() -> intern()` round-trip) on every call.
        let class_sym_opt = match target.view() {
            ValueView::Instance { class_name, .. } => Some(class_name),
            ValueView::Package(name) => Some(name),
            _ => None,
        };
        let class_name = class_sym_opt.map(|s| s.as_str());
        if let Some(cn) = class_name
            && let Some(class_sym) = class_sym_opt
            && let Some((owner_class, method_def)) =
                self.resolve_method_cached(cn, method, class_sym, method_sym, &args, &target)
        {
            // Ambiguous multi dispatch: two or more candidates matched equally
            // well. Raise X::Multi::Ambiguous instead of silently picking one.
            if self.dispatch_ambiguous {
                self.dispatch_ambiguous = false;
                let sigs = self.format_method_candidate_signatures(cn, method, None);
                return Err(
                    crate::runtime::methods_signature_errors::make_multi_ambiguous_error(
                        method, cn, &sigs,
                    ),
                );
            }
            if let Some(result) = self.check_method_wrap_chain(
                cn,
                owner_class.as_str(),
                method,
                &method_def,
                &target,
                &args,
            ) {
                return result;
            }
            // Resolve to a def carrying compiled bytecode, compiling on demand
            // for methods added after the registration compile pass (e.g.
            // `.^add_method`, ledger §1) so they run as bytecode rather than
            // tree-walking. None → native receiver, keep interpreter fallback.
            let resolved: Option<(
                crate::symbol::Symbol,
                std::sync::Arc<crate::runtime::MethodDef>,
            )> = if method_def.compiled_code.is_some() {
                Some((owner_class, method_def))
            } else if !method_def.body.is_empty() {
                self.populate_uncompiled_method(cn, owner_class.as_str(), method, &args, &target)
            } else {
                None
            };
            if let Some((owner_class, method_def)) = resolved {
                // #8880: control only gets here by being declined, in order, by
                // every probe in this function's prefix and in the opcode's --
                // each of them returns when it claims a call. So arriving here
                // *is* the proof that the prefix is inert for this receiver
                // class and method name, and it is where the lane's memo is
                // written. See `vm_call_method_plain_lane`.
                self.note_plain_method_lane_reached(class_sym, method_sym);
                let cc = method_def.compiled_code.clone().expect("compiled_code set");
                let target_id = match target.view() {
                    ValueView::Instance { id, .. } => Some(id),
                    _ => None,
                };
                let attrs_cell = match target.view() {
                    ValueView::Instance { attributes, .. } => Some(attributes.clone()),
                    _ => None,
                };
                let attributes = match target.view() {
                    ValueView::Instance { attributes, .. } => attributes.to_map(),
                    _ => AttrMap::new(),
                };
                let invocant_for_dispatch = if attributes.is_empty() {
                    Value::package(class_sym)
                } else {
                    target.clone()
                };
                let pushed_dispatch = loan_env!(
                    self,
                    push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
                );
                let invocant = Some(target);
                let empty_fns = CompiledFns::default();
                let fns_ref = method_def.compiled_fns.as_deref().unwrap_or(&empty_fns);
                let method_result = self.call_compiled_method(
                    cn,
                    owner_class.as_str(),
                    method,
                    &method_def,
                    &cc,
                    &attributes,
                    args,
                    invocant,
                    fns_ref,
                );
                if pushed_dispatch {
                    self.pop_method_dispatch();
                }
                self.pop_method_samewith_context();
                let (result, reconciled) = method_result?;
                if let Some(id) = target_id {
                    // Commit only a `:=`-adjusted snapshot (cell-CAS race
                    // avoidance — see the primary dispatch site).
                    if let (Some(m), Some(cell)) = (&reconciled, &attrs_cell) {
                        cell.commit_attrs(m.clone());
                    }
                    if result.is_proxy_value()
                        && !self.in_lvalue_assignment
                        && !Self::method_is_rw_capable(&method_def)
                        && let ValueView::Proxy { fetcher, .. } = result.view()
                    {
                        // Without a `:=` adjustment the returned map is absent —
                        // re-snapshot the live cell for the proxy fetcher.
                        let proxy_attrs = match (&reconciled, &attrs_cell) {
                            (Some(m), _) => m.clone(),
                            (None, Some(cell)) => cell.to_map(),
                            (None, None) => AttrMap::new(),
                        };
                        return loan_env!(self, proxy_fetch(fetcher, None, cn, &proxy_attrs, id));
                    }
                }
                return Ok(result);
            }
        }
        // Guard for the whole "lever A" native block below — see
        // `native_lever_a_user_override`'s doc comment (mut path twin of the
        // non-mut guard in `try_compiled_method_or_interpret_inner`).
        let lever_a_blocked = self.native_lever_a_user_override_sym(&target, method_sym);
        // Native `.subst` over a Str with a simple pattern/replacement (lever A).
        if !lever_a_blocked && let Some(result) = self.try_native_subst(&target, method, &args) {
            return result;
        }
        // Native `.sort` over a plain array with no/simple comparator (lever A).
        if !lever_a_blocked && let Some(result) = self.try_native_sort(&target, method, &args) {
            return result;
        }
        // Native `.min` / `.max` over a plain list, including `:by` blocks.
        if !lever_a_blocked && let Some(result) = self.try_native_extrema(&target, method, &args) {
            return result;
        }
        // Native `.minmax` over a plain list, including `:by` blocks.
        if !lever_a_blocked && let Some(result) = self.try_native_minmax(&target, method, &args) {
            return result;
        }
        // Native `.first` over a plain list (no-adverb forms), including blocks.
        if !lever_a_blocked && let Some(result) = self.try_native_first(&target, method, &args) {
            return result;
        }
        // Native QuantHash coercion `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/
        // `.MixHash` over a list-like aggregate or plain Cool scalar. Variable
        // receivers (`@a.Set`, `$s.Bag`) compile to CallMethodMut and so land on this
        // mut path; the
        // coercion produces a *new* Set/Bag/Mix value and never mutates the
        // receiver variable, so there is no writeback — identical to the non-mut
        // path's native dispatch. Instance/Package receivers fall through.
        if !lever_a_blocked
            && args.is_empty()
            && let Some(result) = self.try_native_quanthash_coerce(&target, method)
        {
            return result;
        }
        // Native `.Map` / `.Hash` coercion for variable receivers (`%h.Map`,
        // `@a.Hash`) — same pure value op as the non-mut path, no writeback.
        if !lever_a_blocked
            && args.is_empty()
            && let Some(result) = Self::try_native_map_hash_coerce(&target, method)
        {
            return result;
        }
        // A real mutable Array's `.Seq` yields its element containers, so it
        // must reach the VM-aware producer before the pure structural coercion
        // snapshots the elements.
        if !lever_a_blocked
            && args.is_empty()
            && method == "Seq"
            && !self.native_lever_a_user_override_sym(&target, method_sym)
            && let Some(result) = self.try_element_container_producer(&target, method, &args)
        {
            return Ok(result);
        }
        // Native `.Seq` coercion for the remaining structural receivers.
        if !lever_a_blocked
            && args.is_empty()
            && method == "Seq"
            && let Some(result) = crate::builtins::seq_coerce::to_seq_structural(&target)
        {
            return Ok(result);
        }
        // Native `.IO` coercion over a Cool scalar for variable receivers
        // (`$s.IO`) — builds a *new* IO::Path and never mutates the receiver, so no
        // writeback. Instance / non-IO Package / aggregate receivers fall through.
        if !lever_a_blocked
            && let Some(result) = self.try_native_io_coercion(&target, method, &args)
        {
            return result;
        }
        // Native `.encode` (Cool scalar -> Buf) / `.decode` (Buf/Blob -> Str) for
        // variable receivers (`$s.encode("utf-16")`) — same pure transformation as
        // the non-mut path; returns a *new* Buf/Str, no writeback.
        if !lever_a_blocked
            && let Some(result) = self.try_native_encode_decode(&target, method, &args)
        {
            return result;
        }
        // TODO: compile to bytecode — native/Buf/Failure method fork, mut (ledger §1).
        // User-defined methods run as bytecode (compiled at registration or on
        // demand above); what remains is native receiver dispatch blocked on
        // ③ state ownership / first-class container Phase 2.
        crate::vm::vm_stats::record_method_fallback(method);
        self.vm_call_method_mut_with_values(target_name, target, method, args)
    }
}
