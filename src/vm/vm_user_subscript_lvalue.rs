//! A subscript assignment or binding through a user `postcircumfix:<[ ]>` /
//! `<{ }>` candidate (#16294).
//!
//! `@a[1.5] = v` with Array::Rounded's `multi sub postcircumfix:<[ ]>(R:D \SELF,
//! Any:D \index) { old-same SELF, index.round }` stores into the element the
//! candidate addresses: rakudo assigns to the container the delegating CORE
//! call returns. The store here is native, so the candidate is run first, in a
//! capture mode, only to learn the index its delegation to the CORE routine
//! used; the native store then goes to that index.

use super::*;

/// What a remembered "no candidate delegated" answer of
/// `remap_index_through_user_postcircumfix` is keyed by.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct SubscriptCaptureKey {
    dispatcher_id: usize,
    kind: u8,
    target_id: (usize, usize),
    index_keys: Vec<Symbol>,
}

impl Interpreter {
    /// Replace the index on top of the stack (`[value, index]`) with the index
    /// a matching user `postcircumfix` candidate hands the CORE subscript
    /// routine, when the target is an object such a candidate is written for.
    /// Leaves the stack alone when no user candidate accepts `(target, index)`
    /// or none delegates to the CORE routine.
    // Cost: O(1) when the target is not an object; otherwise one candidate
    // resolution and one call of it.
    pub(super) fn remap_index_through_user_postcircumfix(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
        target_slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        if self.stack.len() < 2 {
            return Ok(());
        }
        // No candidate can apply without a routine of that name in scope:
        // skip the probing (and its argument clones) for every plain store.
        if !self.user_subscript_candidate_possible(
            crate::lexical_builtin_op::postcircumfix_of_subscript(is_positional),
            is_positional,
        ) {
            return Ok(());
        }
        let Some(target) = target_slot
            .and_then(|slot| self.locals.get(slot as usize).cloned())
            .or_else(|| self.env().get_sym(code.const_sym(name_idx)).cloned())
            .map(|value| value.deref_container())
        else {
            return Ok(());
        };
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..) | ValueView::Array(..)
        ) {
            return Ok(());
        }
        let index = self.stack[self.stack.len() - 1].clone();
        let op_name = crate::lexical_builtin_op::postcircumfix_of_subscript(is_positional);
        // Whether a candidate delegates to the CORE routine is a property of
        // the candidate set and of the kinds of the target and the index, so
        // once a run for those found no delegation, the same kinds skip the
        // resolution and the run (a CArray store paid ~1M Ir for NativeCall's
        // `postcircumfix:<[ ]>(CArray:D \array, $pos)`, which only reads
        // `array.AT-POS`, #16319).
        let memo_key = self.subscript_capture_memo_key(op_name, &target, &index);
        if let Some((key, _)) = &memo_key
            && self.caches.subscript_no_delegation.contains_key(key)
        {
            return Ok(());
        }
        let args = vec![target, index];
        let outer = self.dispatch.subscript_lvalue_capture.replace(None);
        let result = self.call_user_postcircumfix(op_name, args);
        let captured = std::mem::replace(&mut self.dispatch.subscript_lvalue_capture, outer);
        result?;
        match captured {
            Some(Some(index)) => {
                let top = self.stack.len() - 1;
                self.stack[top] = index;
            }
            _ => {
                if let Some((key, keepalive)) = memo_key {
                    let memo = &mut self.caches.subscript_no_delegation;
                    if memo.len() >= 1024 {
                        memo.clear();
                    }
                    memo.insert(key, keepalive);
                }
            }
        }
        Ok(())
    }

    /// Whether any user `postcircumfix` routine named `op_name` could answer a
    /// subscript store: a candidate set a block's `use` imported, a family a
    /// `sub EXPORT` hook handed the unit, or a routine of that name in the
    /// function table (the three places [`Self::call_user_postcircumfix`]
    /// looks). `false` proves that call would find nothing.
    // Cost: O(1) with no operator scope open and an unchanged function table;
    // O(f) once per function-table version, f = registered routines.
    fn user_subscript_candidate_possible(&self, op_name: &str, is_positional: bool) -> bool {
        if self.lexical_operator_in_scope(op_name) {
            return true;
        }
        // The two operator symbols are interned once: a store pays no string
        // hash for the family probe.
        static OP_SYMS: std::sync::OnceLock<[Symbol; 2]> = std::sync::OnceLock::new();
        let syms = OP_SYMS.get_or_init(|| {
            [
                Symbol::intern(crate::lexical_builtin_op::postcircumfix_of_subscript(false)),
                Symbol::intern(crate::lexical_builtin_op::postcircumfix_of_subscript(true)),
            ]
        });
        let op_sym = syms[usize::from(is_positional)];
        debug_assert_eq!(op_sym.as_str(), op_name);
        if self.unit_visible_captured_family(op_sym).is_some() {
            return true;
        }
        let version = self.registry().functions.version();
        let (known_bit, answer_bit) = if is_positional {
            (1u8, 2u8)
        } else {
            (4u8, 8u8)
        };
        let (cached_version, bits) = self.caches.user_subscript_routine_gate.get();
        if cached_version == version && bits & known_bit != 0 {
            return bits & answer_bit != 0;
        }
        let present = self
            .registry()
            .functions
            .keys()
            .any(|key| key.as_str().contains(op_name));
        let mut bits = if cached_version == version { bits } else { 0 };
        bits |= known_bit | if present { answer_bit } else { 0 };
        self.caches.user_subscript_routine_gate.set((version, bits));
        present
    }

    /// The key a "no candidate delegated" answer is remembered under, with the
    /// values whose addresses it contains (kept alive by the entry): the
    /// lexical dispatcher in scope, the target's class (or its composition
    /// node) and the index's type key. `None` for a shape that is not
    /// remembered: no lexical dispatcher in scope, an `Array` target, or an
    /// index without a type key.
    // Cost: O(s + 1), s = nested operator scopes.
    fn subscript_capture_memo_key(
        &mut self,
        op_name: &str,
        target: &Value,
        index: &Value,
    ) -> Option<(SubscriptCaptureKey, Vec<Value>)> {
        let dispatcher = self.lexical_operator_dispatcher(op_name)?.clone();
        let ValueView::Sub(data) = dispatcher.view() else {
            return None;
        };
        let dispatcher_id = &*data as *const _ as usize;
        let (kind, target_id) = match target.view() {
            ValueView::Instance { class_name, .. } => (1u8, (class_name.id() as usize, 0)),
            ValueView::Mixin(inner, mixins) => match inner.view() {
                ValueView::Instance { class_name, .. } => (
                    2u8,
                    (
                        &**mixins as *const crate::value::MixinOverrides as usize,
                        class_name.id() as usize,
                    ),
                ),
                _ => return None,
            },
            _ => return None,
        };
        let index_keys = self.multi_arg_type_keys_with_named(std::slice::from_ref(index))?;
        Some((
            SubscriptCaptureKey {
                dispatcher_id,
                kind,
                target_id,
                index_keys,
            },
            vec![dispatcher, target.clone()],
        ))
    }

    /// Call the user `postcircumfix` candidate that accepts `args`, the way
    /// the read side finds it: the candidates a block's `use` imported, a
    /// family a `sub EXPORT` hook handed out, or a registered one whose
    /// invocant is typed with a user class (the CORE candidates take an
    /// untyped invocant, so only such a candidate outranks them).
    // Cost: one candidate resolution and one call of it.
    fn call_user_postcircumfix(
        &mut self,
        op_name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        if let Some(result) = self.try_lexical_operator_candidates(op_name, args.clone()) {
            result?;
            return Ok(());
        }
        if self
            .try_exported_captured_family(op_name, args.clone())?
            .is_some()
        {
            return Ok(());
        }
        if let Some(def) = self.resolve_function_with_types(op_name, &args)?
            && def.param_defs.first().is_some_and(|p| {
                p.type_constraint.as_deref().is_some_and(|t| {
                    let t = t.trim_end_matches([':', 'D', 'U', '?']);
                    !t.is_empty() && !Self::is_builtin_type(t)
                })
            })
        {
            self.call_routine_def(&def, args)?;
        }
        Ok(())
    }
}
