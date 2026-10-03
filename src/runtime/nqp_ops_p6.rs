//! The Rakudo-only `p6*` `nqp::` ops that run as ordinary value ops (#11505).
//!
//! These are not in NQP's op reference: Rakudo registers them for the `Raku`
//! HLL in `src/vm/moar/Perl6/Ops.nqp`, mostly as thin QAST desugars over its
//! `Binder` (BOOTSTRAP) and its return-value handling. Each one here shares the
//! routine mutsu already runs for the same job — the signature binder, the
//! multi-dispatch type check, the return-type check, the context snapshots of
//! `nqp::ctx` — rather than keeping a second copy.
//!
//! The `p6*` ops that are control flow or need the operand's container rather
//! than its value (`p6store`, `p6sink`, `p6return`, `p6invokeflat`) are
//! compile-time forms instead, in `compiler::nqp_forms`.
//!
//! A link of the chained `nqp::` tables (`... -> nqp_ops_coerce -> here ->
//! nativecall_nqp`).

use super::*;
use crate::ast::ParamDef;
use crate::value::ValueView;
use std::sync::Arc;

/// `Binder.trial_bind`'s three answers.
const TRIAL_BIND_NOT_SURE: i64 = 0;
const TRIAL_BIND_OK: i64 = 1;
const TRIAL_BIND_NO_WAY: i64 = -1;

/// MoarVM's argument prim-spec codes, as `nqp::p6trialbind`'s `$sigflags`
/// list carries them (the low nibble of each entry).
const BIND_VAL_INT: i64 = 1;
const BIND_VAL_NUM: i64 = 2;
const BIND_VAL_STR: i64 = 3;
const BIND_VAL_UINT: i64 = 10;

/// An operand with any argument wrapper and container stripped.
fn operand(args: &[Value], i: usize) -> Value {
    crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
        .deref_container()
}

/// The type name a value answers `.WHAT` with, for the type checks below.
fn what_name(v: &Value) -> String {
    match v.view() {
        ValueView::Package(name) => name.resolve().to_string(),
        ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
        _ => crate::runtime::utils::value_type_name(v).to_string(),
    }
}

/// Whether a parameter's declared type is a native one (`int $x`, `str $s`).
fn native_param_family(p: &ParamDef) -> Option<&'static str> {
    crate::native_types::native_family(p.type_constraint.as_deref()?)
}

impl Interpreter {
    /// Try a Rakudo `p6*` value op. `None` means "not an op this table
    /// knows"; the caller then tries the FFI table, the last link.
    pub(crate) fn call_nqp_op_p6(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::p6definite($v): `Bool` — whether the decontainerized value
            // is concrete (`decont` + `isconcrete` + `hllbool`). Concreteness,
            // not `.defined`: a Failure is concrete.
            // Cost: O(1).
            "p6definite" => Ok(Value::truth(crate::runtime::types::value_is_concrete(
                &operand(args, 0),
            ))),
            // nqp::p6box($v): the value as an HLL object. A native reaches the
            // `nqp::` layer already boxed (`nqp::unbox_i` yields an `Int`), so
            // this is the value itself, as Rakudo's `p6box` of an object is.
            // Cost: O(1).
            "p6box" => Ok(operand(args, 0)),
            // nqp::p6decontrv($code, $value) / nqp::p6decontrv_6c: the value a
            // routine returns. An `is rw` routine hands back its container
            // untouched; any other one returns the decontainerized value.
            // (The two differ only in a 6.c-era Proxy rule mutsu's return path
            // does not distinguish.)
            // Cost: O(1).
            "p6decontrv" | "p6decontrv_6c" => self.nqp_p6decontrv(args),
            // nqp::p6typecheckrv($value, $code): check a return value against
            // the routine's declared return type, through the same check a
            // `return` takes (Nil and Failure pass, subsets, coercions).
            // Cost: O(c), c = cost of one type check against the return type.
            "p6typecheckrv" => self.nqp_p6typecheckrv(args),
            // nqp::p6bindassert($value, $type): `$value`, or the
            // X::TypeCheck::Binding a `:=` to a `$type`-typed name raises.
            // Cost: O(c), c = cost of one type check against `$type`.
            "p6bindassert" => self.nqp_p6bindassert(args),
            // nqp::p6isbindable($sig, $capture): 1 when the capture binds to
            // the signature, 0 when it does not — a dry run of the binder.
            // Cost: O(p + a), p = parameters, a = arguments (plus their checks).
            "p6isbindable" => self.nqp_p6isbindable(args),
            // nqp::p6bindcaptosig($sig, $capture): bind the capture to the
            // signature in the current scope; the signature, or the binder's
            // exception.
            // Cost: O(p + a), p = parameters, a = arguments (plus their checks).
            "p6bindcaptosig" => self.nqp_p6bindcaptosig(args),
            // nqp::p6trialbind($sig, @args, @sigflags): Rakudo's compile-time
            // bind analysis — 1 (always binds), -1 (never can), 0 (decided at
            // run time).
            // Cost: O(p), p = parameters (plus one type check each).
            "p6trialbind" => self.nqp_p6trialbind(args),
            // nqp::p6capturelex($code): attach the code object to the current
            // lexical scope and return it. A mutsu closure captures its scope
            // when it is created, so the code object is already attached and
            // comes back as it is (Rakudo also returns any non-code operand
            // unchanged).
            // Cost: O(1).
            "p6capturelex" => Ok(args.first().cloned().unwrap_or(Value::NIL)),
            // nqp::p6getouterctx($code): the context the code object closes
            // over, readable with `nqp::ctxlexpad` like any `nqp::ctx`.
            // Cost: O(v), v = variables the closure's scope holds.
            "p6getouterctx" => self.nqp_p6getouterctx(args),
            // nqp::p6setautothreader($callable): Rakudo's `Binder.set_autothreader`,
            // which records the callable and returns it. Rakudo's binder has
            // no reader of that slot any more (Junction autothreading runs in
            // its dispatchers, as it runs in mutsu's), so the returned callable
            // is the whole observable effect.
            // Cost: O(1).
            "p6setautothreader" => Ok(args.first().cloned().unwrap_or(Value::NIL)),
            _ => return self.call_nqp_op_ffi(op, args),
        })
    }

    fn nqp_p6decontrv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = operand(args, 0);
        let is_rw = match code.view() {
            ValueView::Sub(data) => data.is_rw,
            ValueView::Routine { .. } => false,
            _ => {
                return Err(RuntimeError::new(format!(
                    "No such method 'rw' for invocant of type '{}'",
                    what_name(&code)
                )));
            }
        };
        let value = args.get(1).cloned().unwrap_or(Value::NIL);
        if is_rw {
            Ok(value)
        } else {
            Ok(crate::runtime::types::unwrap_varref_value(value).deref_container())
        }
    }

    fn nqp_p6typecheckrv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = operand(args, 0);
        let code = operand(args, 1);
        let spec = match code.view() {
            ValueView::Sub(_) => self.callable_return_type(&code),
            ValueView::Routine { name, .. } => self.routine_return_spec_by_name(&name.resolve()),
            _ => {
                return Err(RuntimeError::new(format!(
                    "p6typecheckrv expects a Code object, got {}",
                    what_name(&code)
                )));
            }
        };
        match spec {
            // A definite return (`--> 42`) declares a value, not a type: there
            // is nothing to check the returned value against.
            Some(spec) if !self.is_definite_return_spec(&spec) => {
                self.enforce_return_type_constraint(&spec, value)
            }
            _ => Ok(value),
        }
    }

    fn nqp_p6bindassert(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = operand(args, 0);
        let type_name = what_name(&operand(args, 1));
        if self.type_matches_value(&type_name, &value) {
            Ok(value)
        } else {
            Err(self.type_check_binding_failure(&type_name, &value))
        }
    }

    /// The declared parameters of a Signature operand, with their names as
    /// the binder wants them alongside.
    fn nqp_signature_params(
        op: &str,
        sig: &Value,
    ) -> Result<(Arc<Vec<ParamDef>>, Vec<String>), RuntimeError> {
        let defs = crate::value::signature::extract_sig_info(sig)
            .and_then(|info| info.param_defs)
            .ok_or_else(|| {
                RuntimeError::new(format!(
                    "{op} expects a declared Signature, got {}",
                    what_name(sig)
                ))
            })?;
        let names = defs.iter().map(|p| p.name.clone()).collect();
        Ok((defs, names))
    }

    /// A capture operand as the binder's argument list: the positionals, then
    /// each named argument as a `Pair`.
    fn nqp_capture_args(op: &str, capture: &Value) -> Result<Vec<Value>, RuntimeError> {
        let (positional, named) = Self::signature_capture_like(capture).ok_or_else(|| {
            RuntimeError::new(format!(
                "{op} expects a Capture, got {}",
                what_name(capture)
            ))
        })?;
        let mut args = positional;
        args.extend(named.iter().map(|(k, v)| Value::pair(k.clone(), v.clone())));
        Ok(args)
    }

    fn nqp_p6isbindable(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (defs, names) = Self::nqp_signature_params("p6isbindable", &operand(args, 0))?;
        let call_args = Self::nqp_capture_args("p6isbindable", &operand(args, 1))?;
        // A dry run: whatever the binder declared is dropped with the scratch
        // scope (the env is a shared, copy-on-write tier, so this is cheap).
        let saved = self.env.clone();
        let bound = self.bind_function_args_values(&defs, &names, &call_args);
        self.env = saved;
        Ok(Value::int(i64::from(bound.is_ok())))
    }

    fn nqp_p6bindcaptosig(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sig = operand(args, 0);
        let (defs, names) = Self::nqp_signature_params("p6bindcaptosig", &sig)?;
        let call_args = Self::nqp_capture_args("p6bindcaptosig", &operand(args, 1))?;
        self.bind_function_args_values(&defs, &names, &call_args)?;
        Ok(sig)
    }

    fn nqp_p6getouterctx(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = operand(args, 0);
        match code.view() {
            ValueView::Sub(data) => Ok(self.nqp_ctx_of_env(&data.env)),
            _ => Err(RuntimeError::new(format!(
                "p6getouterctx expects a Code object, got {}",
                what_name(&code)
            ))),
        }
    }

    /// `Binder.trial_bind`: decide, without running anything, whether
    /// positional arguments of the given types bind to `$sig`. Only plain
    /// positional parameters are analysed; anything else answers "not sure",
    /// which is always safe (the call is then bound at run time). A type
    /// mismatch is checked with the multi-dispatcher's own
    /// `type_matches_value`, and is "no way" only when the parameter type is
    /// not even a subtype of the argument's type.
    fn nqp_p6trialbind(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (defs, _) = Self::nqp_signature_params("p6trialbind", &operand(args, 0))?;
        let pos_args = Self::nqp_elems_of(&operand(args, 1)).unwrap_or_default();
        let sigflags: Vec<i64> = Self::nqp_elems_of(&operand(args, 2))
            .unwrap_or_default()
            .iter()
            .map(crate::runtime::to_int)
            .collect();
        Ok(Value::int(self.trial_bind(&defs, &pos_args, &sigflags)))
    }

    fn trial_bind(&mut self, defs: &[ParamDef], pos_args: &[Value], sigflags: &[i64]) -> i64 {
        let is_capture = |p: &ParamDef| p.slurpy && (p.name == "_capture" || p.sigilless);
        // A lone capture parameter takes anything (the common proto shape).
        if let [only] = defs
            && is_capture(only)
            && only.where_constraint.is_none()
        {
            return TRIAL_BIND_OK;
        }
        let mut cur = 0usize;
        for p in defs {
            // A slurpy named parameter takes whatever is left of the nameds.
            if p.slurpy && p.name.starts_with('%') && p.where_constraint.is_none() {
                continue;
            }
            let plain_trait = |t: &String| matches!(t.as_str(), "raw" | "copy");
            let type_name = p.type_constraint.as_deref().unwrap_or("Mu");
            // Anything but a boring positional is left to the run-time bind:
            // nameds, slurpies, captures, `is rw`, post-constraints, sub-
            // signatures, type captures, definedness smileys and coercions.
            // (Rakudo's own test of the post-constraint/named/type-capture
            // trio joins them with `||` inside an `unless`, so it bails out
            // only when all three are present and answers "always binds" for
            // `$a where 1`; here any one of them makes the answer "not sure",
            // the only answer that is right for every argument value.)
            if p.named
                || p.slurpy
                || p.sigilless
                || !p.traits.iter().all(plain_trait)
                || p.where_constraint.is_some()
                || p.literal_value.is_some()
                || p.sub_signature.is_some()
                || p.type_capture.is_some()
                || type_name.contains(':')
                || type_name.contains('(')
            {
                return TRIAL_BIND_NOT_SURE;
            }
            let Some(arg) = pos_args.get(cur) else {
                if p.optional_marker || p.default.is_some() {
                    cur += 1;
                    continue;
                }
                return TRIAL_BIND_NO_WAY;
            };
            let got_prim = sigflags.get(cur).copied().unwrap_or(0) & 0xF;
            if let Some(family) = native_param_family(p) {
                if got_prim != 0 {
                    let fits = match family {
                        "str" => got_prim == BIND_VAL_STR,
                        "num" => got_prim == BIND_VAL_NUM,
                        "uint" => got_prim == BIND_VAL_UINT || got_prim == BIND_VAL_INT,
                        _ => got_prim == BIND_VAL_INT,
                    };
                    if !fits {
                        return TRIAL_BIND_NO_WAY;
                    }
                } else {
                    // An object argument for a native parameter: whether it
                    // unboxes is decided at run time (Rakudo's `isint`-style
                    // probes are true only of natives, which a list never
                    // holds).
                    return TRIAL_BIND_NOT_SURE;
                }
            } else {
                let arg = match got_prim {
                    0 => arg.clone(),
                    BIND_VAL_STR => Value::package(Symbol::intern("Str")),
                    BIND_VAL_INT | BIND_VAL_UINT => Value::package(Symbol::intern("Int")),
                    _ => Value::package(Symbol::intern("Num")),
                };
                if type_name != "Mu" && !self.type_matches_value(type_name, &arg) {
                    // A Junction may still autothread at run time.
                    if arg.is_junction_value() {
                        return TRIAL_BIND_NOT_SURE;
                    }
                    // `Any` might hold an `Int` at run time; an `Int` will
                    // never be a `Str`.
                    let arg_type = what_name(&arg);
                    let param_type = self.type_arg_value_from_name(type_name);
                    return if self.type_matches_value(&arg_type, &param_type) {
                        TRIAL_BIND_NOT_SURE
                    } else {
                        TRIAL_BIND_NO_WAY
                    };
                }
            }
            cur += 1;
        }
        if cur < pos_args.len() {
            TRIAL_BIND_NO_WAY
        } else {
            TRIAL_BIND_OK
        }
    }
}
