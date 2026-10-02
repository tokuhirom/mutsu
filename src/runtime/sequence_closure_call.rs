//! Calling the closures of the sequence operator (`...`): the generator that
//! produces the next element and the endpoint matcher that stops it.
//!
//! Both are ordinary `Callable`s, so both run through the VM's value-call
//! path ([`Interpreter::vm_call_on_value`]), which executes the closure's own
//! `compiled_code`. Neither is ever re-compiled from its AST body: doing that
//! once per element cost one `Compiler::compile` per generated element (#10119).
//!
//! How many trailing elements a closure receives is Raku's `.count` of that
//! closure ([`SeqClosureCount`]), read once per sequence.
use super::*;
use crate::value::{SeqClosureCount, SeqGeneratorShape};

impl Interpreter {
    /// Raku's `.count` of a sequence generator/endpoint closure: the number of
    /// trailing elements each call receives, or all of them for a slurpy.
    // Cost: O(p), p = the closure's parameters (one signature read, done once per sequence).
    pub(super) fn sequence_closure_count(&self, callable: &Value) -> SeqClosureCount {
        let ValueView::Sub(data) = callable.view() else {
            return SeqClosureCount::Fixed(1);
        };
        let sig = self.sub_signature_value(&data);
        match crate::value::signature::extract_sig_info(&sig)
            .map(|info| Self::signature_positional_count(&info))
        {
            Some(Some(n)) => SeqClosureCount::Fixed(usize::try_from(n).unwrap_or(0)),
            Some(None) => SeqClosureCount::All,
            None => SeqClosureCount::Fixed(1),
        }
    }

    /// Whether the endpoint closure matches `candidate`, the element that would
    /// follow `before` (every element so far). A closure taking more elements
    /// than exist yet is not consulted, as in Rakudo.
    // Cost: O(c) + one closure call, c = elements passed (the closure's count, or every element so far for a slurpy).
    pub(super) fn sequence_endpoint_matches(
        &mut self,
        endpoint: &Value,
        count: SeqClosureCount,
        before: &[Value],
        candidate: &Value,
    ) -> Result<bool, RuntimeError> {
        let tail = match count {
            SeqClosureCount::All => before,
            SeqClosureCount::Fixed(0) => &[],
            SeqClosureCount::Fixed(n) if before.len() + 1 < n => return Ok(false),
            SeqClosureCount::Fixed(n) => &before[before.len() + 1 - n..],
        };
        let mut args = Vec::with_capacity(tail.len() + 1);
        args.extend(tail.iter().cloned());
        if count != SeqClosureCount::Fixed(0) {
            args.push(candidate.clone());
        }
        let matched = match self.vm_call_on_value(endpoint.clone(), args, None) {
            Ok(v) => v,
            Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
            Err(e) => return Err(e),
        };
        Ok(matched.truthy())
    }

    /// How a sequence generator is called, decided once per sequence: a named
    /// routine (`&infix:<+>`, a user `sub`) takes its arity from every
    /// registered candidate; any other closure takes its `.count`.
    // Cost: O(f + p), f = registered functions (one scan per sequence), p = the closure's parameters.
    pub(crate) fn sequence_generator_shape(&self, generator: &Value) -> SeqGeneratorShape {
        let (package, name) = match generator.view() {
            ValueView::Sub(data) => (data.package.resolve(), data.name.resolve()),
            ValueView::Routine { package, name, .. } => (package.resolve(), name.resolve()),
            _ => return SeqGeneratorShape::Closure(SeqClosureCount::Fixed(1)),
        };
        if matches!(generator.view(), ValueView::Routine { .. })
            || self.sequence_has_registered_routine(&package, &name)
        {
            self.sequence_routine_shape(&package, &name)
        } else {
            SeqGeneratorShape::Closure(self.sequence_closure_count(generator))
        }
    }

    /// Produce the next element of a closure-based sequence given the current
    /// element `history`. Returns `Ok(Some(v))` for the next value, or
    /// `Ok(None)` when the generator signalled termination (`last`, or a
    /// suppressed error).
    ///
    /// This is the single per-element step shared by the initial generation
    /// loop in `eval_sequence` and the on-demand extension of an infinite
    /// closure sequence (`Interpreter::extend_closure_sequence`).
    // Cost: O(c) + one generator call, c = elements passed (the generator's count, or the whole history for a slurpy).
    pub(crate) fn sequence_closure_step(
        &mut self,
        generator: &Value,
        history: &[Value],
        shape: SeqGeneratorShape,
        suppress_generator_error: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        let routine_args = match shape {
            SeqGeneratorShape::Closure(count) => {
                // At most `count` trailing elements; fewer when the history is
                // shorter (Rakudo trims its tail to the count), so a closure
                // whose parameters are optional (`{ ++$i } ... *` with no
                // seeds) is called with what exists and a required one fails
                // in the binder.
                let tail = match count {
                    SeqClosureCount::All => history,
                    SeqClosureCount::Fixed(n) => &history[history.len().saturating_sub(n)..],
                };
                let args = tail.iter().map(Self::normalize_sequence_arg).collect();
                let result = self.vm_call_on_value(generator.clone(), args, None);
                return Self::sequence_step_result(result, suppress_generator_error);
            }
            SeqGeneratorShape::RoutineFixed(arity) => {
                Self::collect_sequence_args_fixed(history, arity)?
            }
            SeqGeneratorShape::RoutineSlurpy { min_arity } => {
                Self::collect_sequence_args_slurpy(history, min_arity)
            }
        };
        let result = match generator.view() {
            ValueView::Sub(data) => {
                let name = data.name.resolve();
                self.call_function(name.strip_prefix('&').unwrap_or(&name), routine_args)
            }
            _ => self.call_sub_value(generator.clone(), routine_args, false),
        };
        Self::sequence_step_result(result, suppress_generator_error)
    }

    fn sequence_step_result(
        result: Result<Value, RuntimeError>,
        suppress_generator_error: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        match result {
            Ok(v) => Ok(Some(v)),
            Err(e) if e.return_value.is_some() => Ok(e.return_value),
            Err(e) if e.is_last() => Ok(None),
            Err(_e) if suppress_generator_error => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn collect_sequence_args_fixed(
        result: &[Value],
        arity: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        if arity == 0 {
            return Ok(Vec::new());
        }
        if result.len() < arity {
            return Err(RuntimeError::new(format!(
                "Too few positionals passed; expected {arity} arguments but got {}",
                result.len()
            )));
        }
        Ok(result[result.len() - arity..]
            .iter()
            .map(Self::normalize_sequence_arg)
            .collect())
    }

    fn collect_sequence_args_slurpy(result: &[Value], min_arity: usize) -> Vec<Value> {
        if result.len() >= min_arity {
            return result.iter().map(Self::normalize_sequence_arg).collect();
        }
        let mut args = vec![Value::NIL; min_arity - result.len()];
        args.extend(result.iter().map(Self::normalize_sequence_arg));
        args
    }

    /// The arity of a named-routine generator, from every registered candidate.
    fn sequence_routine_shape(&self, package: &str, name: &str) -> SeqGeneratorShape {
        let name = name.strip_prefix('&').unwrap_or(name);
        if name.starts_with("prefix:<") || name.starts_with("postfix:<") {
            return SeqGeneratorShape::RoutineFixed(1);
        }
        if name.starts_with("infix:<") {
            return SeqGeneratorShape::RoutineFixed(2);
        }

        let local_prefix = format!("{package}::{name}/");
        let global_prefix = format!("GLOBAL::{name}/");
        let mut fixed_arity = 0usize;
        let mut slurpy_min: Option<usize> = None;

        for (key, def) in self.registry().functions.iter() {
            let key_s = key.resolve();
            let loose_match = key_s.contains(&format!("::{name}/"));
            if !key_s.starts_with(&local_prefix)
                && !key_s.starts_with(&global_prefix)
                && !loose_match
            {
                continue;
            }

            let mut positional_non_slurpy = 0usize;
            let mut has_slurpy = false;
            if def.param_defs.is_empty() {
                positional_non_slurpy = def.params.len();
            } else {
                for pd in &def.param_defs {
                    if pd.named {
                        continue;
                    }
                    if pd.slurpy {
                        has_slurpy = true;
                    } else {
                        positional_non_slurpy += 1;
                    }
                }
            }

            if has_slurpy {
                slurpy_min = Some(match slurpy_min {
                    Some(existing) => existing.max(positional_non_slurpy),
                    None => positional_non_slurpy,
                });
            } else {
                fixed_arity = fixed_arity.max(positional_non_slurpy);
            }
        }

        if let Some(min) = slurpy_min {
            SeqGeneratorShape::RoutineSlurpy { min_arity: min }
        } else if fixed_arity > 0 {
            SeqGeneratorShape::RoutineFixed(fixed_arity)
        } else {
            SeqGeneratorShape::RoutineFixed(2)
        }
    }

    fn sequence_has_registered_routine(&self, package: &str, name: &str) -> bool {
        let name = name.strip_prefix('&').unwrap_or(name);
        let local_prefix = format!("{package}::{name}/");
        let global_prefix = format!("GLOBAL::{name}/");
        self.registry().functions.keys().any(|key| {
            let ks = key.resolve();
            ks.starts_with(&local_prefix)
                || ks.starts_with(&global_prefix)
                || ks.contains(&format!("::{name}/"))
        })
    }
}
