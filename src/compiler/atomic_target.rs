//! The target of an integer atomic (`⚛++`, `⚛+=`, `atomic-fetch-add`,
//! `nqp::atomicinc_i`, ...), as far as the lexical scope chain can tell (#11834).
//!
//! Rakudo gives these routines one candidate, `atomicint $target is rw`, so the
//! target has to be a native-integer container: an `int` / `atomicint`
//! variable or attribute, or an element of a native-int array. A plain
//! `my $x` is a dispatch failure.
//!
//! Whether a *lexical* is such a container is settled by its declaration, which
//! is visible to the compiler -- through the enclosing scopes of a nested sub
//! or closure as well, which is what makes a module-scope `my atomicint $hits`
//! reachable from an exported routine running on a worker thread. The runtime
//! cannot answer that question from the name alone: the type metadata is keyed
//! by name in a frame's environment and does not follow a routine into another
//! module or thread.
//!
//! The compiler therefore classifies the target and emits a guard only when it
//! cannot prove the target native:
//!
//! - **declared native** (`my int $x`, `my atomicint $x`): no guard at all, so
//!   the hot `$n⚛++` loop is exactly what it was;
//! - **declared, not native** (`my $x`, `my Int $x`, `my uint $x`): a guard that
//!   always refuses;
//! - **not decided by a declaration** (a parameter, an attribute, a `:=` alias,
//!   an array or hash element, an outer name this compilation cannot see): a
//!   guard that asks the container at run time
//!   (`Interpreter::builtin_atomic_int_target`).
//!
//! The guard is a pass-through call, `__mutsu_atomic_int_target(target,
//! declared, spelling[, operand])`, which answers its operand, so it slots in
//! between pushing the target name and the helper call without disturbing the
//! stack. The one check lives in `runtime/builtins_atomic_target.rs`.
//!
//! The helper itself finds its binding from the same lexical knowledge: the
//! target it is handed is the name tagged with the frame slot the call site
//! reaches ([`Compiler::emit_atomic_target`]), because a name alone cannot tell
//! an inner `my atomicint $y` from the outer `$y` it shadows (#12006).

use super::*;

/// What the lexical scope chain says about an integer atomic's scalar target.
pub(super) enum AtomicTargetDecl {
    /// No visible declaration decides it.
    Unknown,
    /// A plain `my`/`state`/`our` declaration with this declared type (`None`
    /// when it is untyped).
    Declared(Option<String>),
}

/// How an atomic routine is spelled at its call site, and whether it refuses a
/// non-native target even though its Raku form would take any `$target is rw`.
///
/// The spelling is what a refusal names: `$x⚛++` is `postfix:<⚛++>` however it
/// is lowered. The `nqp::` `_i` loads, stores and `cas_i` lower to
/// `atomic-fetch` / `atomic-assign` / `cas`, which accept any scalar, but the
/// nqp ops they stand for do not.
#[derive(Clone, Debug)]
pub(super) struct AtomicSpelling {
    pub(super) display: String,
    pub(super) int_only: bool,
}

/// The integer atomic routines and operators (an operator is a call of its
/// Rakudo routine, `postfix:<⚛++>($x)`) and the helper each is an application
/// of: `(routine, helper, takes an operand, negates it)`. Subtraction is the
/// add helper with the operand negated.
const INT_ATOMIC_ROUTINES: &[(&str, &str, bool, bool)] = &[
    ("postfix:<⚛++>", "__mutsu_atomic_post_inc_var", false, false),
    ("prefix:<++⚛>", "__mutsu_atomic_pre_inc_var", false, false),
    ("postfix:<⚛-->", "__mutsu_atomic_post_dec_var", false, false),
    ("prefix:<--⚛>", "__mutsu_atomic_pre_dec_var", false, false),
    ("infix:<⚛+=>", "__mutsu_atomic_add_var", true, false),
    ("infix:<⚛-=>", "__mutsu_atomic_add_var", true, true),
    (
        "atomic-fetch-inc",
        "__mutsu_atomic_post_inc_var",
        false,
        false,
    ),
    (
        "atomic-inc-fetch",
        "__mutsu_atomic_pre_inc_var",
        false,
        false,
    ),
    (
        "atomic-fetch-dec",
        "__mutsu_atomic_post_dec_var",
        false,
        false,
    ),
    (
        "atomic-dec-fetch",
        "__mutsu_atomic_pre_dec_var",
        false,
        false,
    ),
    (
        "atomic-fetch-add",
        "__mutsu_atomic_fetch_add_var",
        true,
        false,
    ),
    ("atomic-add-fetch", "__mutsu_atomic_add_var", true, false),
    (
        "atomic-fetch-sub",
        "__mutsu_atomic_fetch_add_var",
        true,
        true,
    ),
    ("atomic-sub-fetch", "__mutsu_atomic_add_var", true, true),
];

impl Compiler {
    /// The helper behind the integer atomic routine `name`, whether it takes an
    /// operand, and whether the operand is negated.
    pub(super) fn int_atomic_routine(name: &str) -> Option<(&'static str, bool, bool)> {
        INT_ATOMIC_ROUTINES
            .iter()
            .find(|(routine, ..)| *routine == name)
            .map(|(_, helper, has_operand, negate)| (*helper, *has_operand, *negate))
    }

    /// Record that the current scope declares the plain scalar `name` with the
    /// declared type `ty`. A name with a twigil, a sigil other than `$`, or a
    /// package qualifier is not a plain scalar and is left unrecorded.
    pub(super) fn record_scalar_decl_type(&mut self, name: &str, ty: Option<&str>) {
        let plain = name
            .chars()
            .next()
            .is_some_and(|c| c.is_alphabetic() || c == '_')
            && !name.contains("::");
        if !plain {
            return;
        }
        let ty = ty.map(|t| self.resolve_type_alias_constraint(t));
        if let Some(frame) = self.local_scopes.last_mut() {
            frame.record_scalar_type(name, ty.as_deref());
        }
    }

    /// Record the declared type of each plain scalar parameter, so an integer
    /// atomic on it is classified like one on a `my` of the same type.
    ///
    /// A parameter binds a *value* unless it passes a container through
    /// (`is rw`, `is raw`, `\x`): `sub f($p) { $p⚛++ }` is refused even when
    /// the caller's variable is an `atomicint`, because `$p` is a read-only
    /// copy. A pass-through parameter takes its container from the caller, so
    /// the runtime asks that container; a native-typed one is native.
    pub(super) fn seed_param_decl_types(&mut self, param_defs: &[crate::ast::ParamDef]) {
        for pd in param_defs {
            if pd.name.is_empty()
                || pd.sigilless
                || pd.slurpy
                || pd.double_slurpy
                || pd.sub_signature.is_some()
                || pd.name.starts_with(['@', '%', '&'])
            {
                continue;
            }
            let ty = pd.type_constraint.as_deref();
            let native = ty.is_some_and(|t| {
                crate::native_types::is_atomic_int_target_type(t)
                    || crate::native_types::is_narrow_atomic_int_type(t)
            });
            let passes_container = pd.traits.iter().any(|t| t == "rw" || t == "raw");
            if native || !passes_container {
                self.record_scalar_decl_type(&pd.name, ty);
            }
        }
    }

    /// What the scope chain at this point says about the scalar `name`.
    // Cost: O(d), d = the depth of the lexical scope chain.
    pub(super) fn atomic_target_decl(&self, name: &str) -> AtomicTargetDecl {
        // Innermost scope first: the first scope that declares the name is the
        // binding the atomic reaches, and if that declaration is not a plain
        // `my` of a type we recorded, nothing outer may answer for it.
        for frame in self
            .local_scopes
            .iter()
            .rev()
            .chain(self.enclosing_scopes.iter().rev())
        {
            if frame.contains_key(name) {
                return match frame.scalar_type(name) {
                    Some(ty) => AtomicTargetDecl::Declared(ty.map(str::to_string)),
                    None => AtomicTargetDecl::Unknown,
                };
            }
        }
        AtomicTargetDecl::Unknown
    }

    /// The slot of the frame-local binding the scalar `name` reaches at this
    /// point, when this very chunk declares it (#12006).
    ///
    /// Two same-named lexicals have two slots (`code.locals == ["y", "y"]`), and
    /// `local_map` always points at the innermost declaration still in scope, so
    /// the compiler knows which one a `$y⚛++` means. A name no active scope of
    /// this chunk declares -- a captured outer lexical, a global, an attribute --
    /// has no slot to name: `local_map` keeps the slots of popped sibling blocks,
    /// and one of those would be a different variable.
    // Cost: O(d), d = the depth of this chunk's lexical scope chain.
    pub(super) fn atomic_target_slot(&self, name: &str) -> Option<u32> {
        if !Self::is_plain_lexical_name(name)
            || !self.local_scopes.iter().any(|f| f.contains_key(name))
        {
            return None;
        }
        self.local_map.get(name).copied()
    }

    /// Push the target of an atomic scalar helper (`__mutsu_atomic_*_var`,
    /// `__mutsu_cas_*var`): the variable's name, tagged with the slot of the
    /// binding the call site reaches when [`Self::atomic_target_slot`] knows it.
    ///
    /// The helpers find their binding by name, and a name cannot tell an inner
    /// `my atomicint $y` from the outer `$y` it shadows. The tag is the
    /// declaring scope's own binding, resolved here once (ADR-0097); an untagged
    /// name keeps the by-name lookup, which is right for a name this chunk does
    /// not declare.
    pub(super) fn emit_atomic_target(&mut self, name: &str) {
        let name_idx = self.code.add_constant(Value::str(name.to_string()));
        self.code.emit(OpCode::LoadConst(name_idx));
        if let Some(slot) = self.atomic_target_slot(name) {
            self.code.emit(OpCode::WrapVarRef { name_idx, slot });
        }
    }

    /// The spelling the enclosing call site was written with, consumed by the
    /// lowering it wraps; `default` names the routine when there was none.
    fn take_atomic_spelling(&mut self, default: &str) -> AtomicSpelling {
        self.atomic_spelling.take().unwrap_or(AtomicSpelling {
            display: default.to_string(),
            int_only: false,
        })
    }

    /// Emit the guard for an integer atomic on the target `target` (a scalar
    /// name, an attribute name `!v` / `.v`, or an element container `@a` /
    /// `%h`), unless the declaration proves the target native.
    ///
    /// With `operand`, the operand is compiled here as the guard's last
    /// argument and is left on the stack as its answer; without one the guard
    /// is a statement and leaves nothing.
    ///
    /// `force` makes a lenient atomic (`atomic-fetch`, ...) check too: that is
    /// the `nqp::` `_i` ops. `display` is the spelling a refusal names.
    pub(super) fn emit_int_atomic_guard(
        &mut self,
        target: &str,
        display: &str,
        operand: Option<&Expr>,
        is_element: bool,
    ) {
        // `NIL` asks the container at run time; a string is the declared type
        // (empty for an untyped declaration) the compiler already knows.
        let declared_value = if is_element {
            Value::NIL
        } else {
            match self.atomic_target_decl(target) {
                AtomicTargetDecl::Declared(ty) => {
                    let native = ty.as_deref().is_some_and(|t| {
                        crate::native_types::is_atomic_int_target_type(
                            crate::runtime::types::strip_type_smiley(t).0,
                        )
                    });
                    if native {
                        // Proven native: no guard, and the hot path is unchanged.
                        if let Some(operand) = operand {
                            self.compile_expr(operand);
                        }
                        return;
                    }
                    Value::str(ty.unwrap_or_default())
                }
                AtomicTargetDecl::Unknown => Value::NIL,
            }
        };
        let target_idx = self.code.add_constant(Value::str(target.to_string()));
        let declared_idx = self.code.add_constant(declared_value);
        let display_idx = self.code.add_constant(Value::str(display.to_string()));
        self.code.emit(OpCode::LoadConst(target_idx));
        self.code.emit(OpCode::LoadConst(declared_idx));
        self.code.emit(OpCode::LoadConst(display_idx));
        let arity = match operand {
            Some(operand) => {
                self.compile_expr(operand);
                4
            }
            None => 3,
        };
        let guard_idx = self
            .code
            .add_constant(Value::str_from("__mutsu_atomic_int_target"));
        self.code.emit(OpCode::CallFunc {
            name_idx: guard_idx,
            arity,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
        if operand.is_none() {
            self.code.emit(OpCode::Pop);
        }
    }

    /// Compile an integer atomic on the scalar or attribute `var_name`:
    /// `helper(var_name[, operand])`, guarded as [`Self::emit_int_atomic_guard`]
    /// describes. `negate` flips the operand (`atomic-fetch-sub` is an add of
    /// the negation).
    pub(super) fn compile_int_atomic_var_call(
        &mut self,
        helper: &str,
        var_name: &str,
        default_display: &str,
        operand: Option<&Expr>,
        negate: bool,
    ) {
        let spelling = self.take_atomic_spelling(default_display);
        self.note_atomic_env_sync_target(var_name, true);
        let call_name_idx = self.code.add_constant(Value::str_from(helper));
        self.emit_atomic_target(var_name);
        self.emit_int_atomic_guard(var_name, &spelling.display, operand, false);
        if operand.is_some() && negate {
            self.code.emit(OpCode::Negate);
        }
        let arity = if operand.is_some() { 2 } else { 1 };
        self.code.emit(OpCode::CallFunc {
            name_idx: call_name_idx,
            arity,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
    }

    /// Run `f` with `spelling` as the spelling of the atomic routine it
    /// compiles.
    pub(super) fn with_atomic_spelling(
        &mut self,
        spelling: AtomicSpelling,
        f: impl FnOnce(&mut Self),
    ) {
        let saved = self.atomic_spelling.replace(spelling);
        f(self);
        self.atomic_spelling = saved;
    }

    /// The guard of a lenient atomic (`atomic-fetch`, `atomic-assign`, `cas`,
    /// `⚛$x`, `$x ⚛= v`), which takes any scalar but not a narrow native integer.
    ///
    /// - An `nqp::` `_i` spelling wants a native integer, so it gets the full
    ///   integer-atomic guard (and consumes the spelling).
    /// - Any other `nqp::` spelling keeps today's behaviour: MoarVM words its
    ///   refusal of those differently (`A IntLexRef container does not know how
    ///   to do an atomic load`), which is not modelled.
    /// - A Raku-level spelling takes the narrow-only guard for a scalar, so the
    ///   plain forms stay legal on any scalar (#12008). An element or an
    ///   attribute is judged by the builtin itself (see
    ///   [`Self::emit_narrow_atomic_guard`]).
    pub(super) fn emit_lenient_atomic_guard(&mut self, target: &str, is_element: bool) {
        // Taken, not just read: the spelling belongs to this one call, and the
        // operands compiled after it must not inherit it.
        match self.atomic_spelling.take() {
            Some(spelling) if spelling.int_only => {
                self.emit_int_atomic_guard(target, &spelling.display, None, is_element);
            }
            Some(_) => {}
            None => self.emit_narrow_atomic_guard(target, is_element),
        }
    }

    /// `__mutsu_atomic_narrow_target(target, declared)` as a statement, unless
    /// the declaration proves the target is not a narrow native integer.
    ///
    /// Like [`Self::emit_int_atomic_guard`], a declared type settles it at
    /// compile time: a plain or machine-size declaration costs nothing, a
    /// narrow one always refuses, and a scalar no declaration decides (a
    /// parameter, an outer name) asks the container at run time.
    pub(super) fn emit_narrow_atomic_guard(&mut self, target: &str, is_element: bool) {
        // An element or an attribute is judged by the lenient builtin itself,
        // from the declared type it already reads for its own coercion
        // (`check_atomic_elem_type`, `atomic_assign_coerced_value`): a guard call
        // of its own added ~30% to a `cas` loop on one.
        if is_element || target.starts_with(['!', '.']) {
            return;
        }
        let declared_value = match self.atomic_target_decl(target) {
            AtomicTargetDecl::Declared(ty) => {
                let narrow = ty.as_deref().is_some_and(|t| {
                    crate::native_types::is_narrow_atomic_int_type(
                        crate::runtime::types::strip_type_smiley(t).0,
                    )
                });
                if !narrow {
                    return;
                }
                Value::str(ty.unwrap_or_default())
            }
            AtomicTargetDecl::Unknown => Value::NIL,
        };
        let target_idx = self.code.add_constant(Value::str(target.to_string()));
        let declared_idx = self.code.add_constant(declared_value);
        self.code.emit(OpCode::LoadConst(target_idx));
        self.code.emit(OpCode::LoadConst(declared_idx));
        let guard_idx = self
            .code
            .add_constant(Value::str_from("__mutsu_atomic_narrow_target"));
        self.code.emit(OpCode::CallFunc {
            name_idx: guard_idx,
            arity: 2,
            arg_sources_idx: None,
            literal_native_args: 0,
            static_arg_types: false,
        });
        self.code.emit(OpCode::Pop);
    }
}
