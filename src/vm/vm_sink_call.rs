//! Whether a sunk statement call's value runs its user `sink` method
//! (`OpCode::SinkPopCall`, #11537).
//!
//! Rakudo sinks the object a call hands back. A routine that is not `is rw`
//! or `is raw` decontainerizes its implicit result, so `sub g { $x }; g;`
//! runs `$x`'s `sink`; a routine that hands back a container (`is rw`,
//! `return-rw`, `return $x`) is not sunk through. mutsu's call result carries
//! no container, so the answer comes from the callee's declaration
//! (`RoutineBodyFacts::result_is_decontainerized` and the traits), asked only
//! once the value has a user `sink` to run.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::ValueView;

impl Interpreter {
    /// Whether the value on the stack top, the result of a statement call to
    /// the routine named by constant `name_idx`, runs its user `sink` method.
    /// `false` for a value without one, for a code variable, for a multi with a
    /// candidate that hands back a container and for a routine that does.
    // Cost: O(1) when the value has no user `sink` (a tag probe, then one
    // method-table lookup for an instance); otherwise O(d) for the method
    // lookup, d = MRO depth, plus one routine lookup by name; a multi adds
    // O(c) for its c candidates.
    pub(crate) fn sunk_call_result_runs_sink(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        name_idx: u32,
    ) -> bool {
        let Some(value) = self.stack.last() else {
            return false;
        };
        let value = value.clone();
        let has_user_sink = match value.view() {
            ValueView::Instance { class_name, .. } => {
                self.has_user_method(class_name.as_str(), "sink")
                    || self.user_find_method_declares(&value)
            }
            _ => value.is_mixin_value() && self.mixin_composes_method(&value, "sink"),
        };
        if !has_user_sink {
            return false;
        }
        // An itemized aggregate came out of a `$` container (`my $r = [1, 2]
        // but R; $r`): rakudo does not sink a `Scalar` holding an `Iterable`
        // that a routine hands back, where a fresh one it `return`s is sunk
        // (#16522).
        if Self::value_is_itemized_aggregate(&value) {
            return false;
        }
        let name = code.constants[name_idx as usize].to_string_value();
        // A routine a block's or unit's `use` bound in its lexical import set
        // (a `sub EXPORT` hook's `'&slack' => &trampoline`) is reached through
        // the `LexImportCall` that precedes the call, not the registry: its
        // declaration travels with the binding (#16331).
        if let Some(crate::opcode::OpCode::LexImportCall { chain_idx }) =
            ip.checked_sub(2).and_then(|at| code.ops.get(at))
            && let Some(chain) = code.lexical_import_chains.get(*chain_idx as usize)
            && let Some(binding) =
                self.lexical_import_binding(chain, crate::symbol::Symbol::intern(&name))
        {
            return match binding {
                crate::runtime::lexical_imports::LexicalImport::Code(callable) => {
                    match callable.view() {
                        ValueView::Sub(data) => {
                            !data.is_rw
                                && !crate::opcode::body_uses_return_rw(&data.body)
                                && if data.is_raw {
                                    crate::ast::result_decont::raw_result_is_fresh_call(&data.body)
                                } else {
                                    crate::ast::result_decont::result_is_decontainerized(
                                        &data.param_defs,
                                        &data.body,
                                    )
                                }
                        }
                        _ => false,
                    }
                }
                crate::runtime::lexical_imports::LexicalImport::Routine(def) => {
                    Self::sinks_declared_result(&def)
                }
            };
        }
        if let Some(def) = self.resolve_function(&name) {
            return Self::sinks_declared_result(&def);
        }
        // A multi: the candidate that ran is not known here, so the result is
        // sunk when every candidate hands back a fresh value. A mixed set (one
        // `is rw` candidate) stays unsunk, the conservative answer.
        let candidates = self.resolve_all_multi_candidates(&name);
        !candidates.is_empty() && candidates.iter().all(|def| Self::sinks_declared_result(def))
    }

    /// Whether `value`, or the aggregate a mixin wraps, is an Array/List/Hash
    /// carrying the item mark a `$` container gives it.
    // Cost: O(m), m = nesting depth of mixins.
    fn value_is_itemized_aggregate(value: &crate::value::Value) -> bool {
        match value.view() {
            ValueView::Mixin(inner, _) => Self::value_is_itemized_aggregate(inner),
            ValueView::Array(_, kind) => kind.is_itemized(),
            ValueView::Hash(_) => value.hash_is_itemized(),
            // An Array/List/Hash subclass instance records the item mark as
            // renderer metadata (`itemize_scalar_store_value`).
            ValueView::Instance { attributes, .. } => attributes
                .contains_key(crate::builtins::methods_0arg::raku_repr::RAKU_SCALAR_ITEMIZED_KEY),
            _ => false,
        }
    }

    /// Whether a call of `def` hands its caller a fresh value (not a
    /// container) that gets sunk.
    // Cost: O(1) after the routine's body facts are computed once.
    fn sinks_declared_result(def: &crate::ast::FunctionDef) -> bool {
        !def.is_rw && {
            let facts = Self::routine_body_facts(def);
            !facts.uses_return_rw
                && if def.is_raw {
                    facts.result_is_fresh_call
                } else {
                    facts.result_is_decontainerized
                }
        }
    }
}

impl Interpreter {
    /// Whether a sunk statement call to the code variable `&name` hands back
    /// the container of its last statement, so the value inside it (a stored
    /// `Failure`, an unsuccessful `Proc`) is not sunk: `my &c = { $z = f() };
    /// c();` throws nothing, where `sub s { $z = f() }; s()` does.
    // Cost: O(k) in the callee's trailing marker statements, plus one code
    // variable lookup by name.
    pub(crate) fn sunk_call_result_is_block_container(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> bool {
        let name = code.constants[name_idx as usize].to_string_value();
        let Some(callable) = self.lexical_amp_var_callable(Some(code), &name) else {
            return false;
        };
        let ValueView::Sub(data) = callable.view() else {
            return false;
        };
        data.compiled_routine.is_none()
            && crate::runtime::utils::value_type_name(&callable) == "Block"
            && crate::ast::result_decont::block_result_is_container(&data.body)
    }
}
