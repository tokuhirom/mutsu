use super::methods_string::SubstCaseTransforms;
use super::*;

impl Interpreter {
    /// Evaluate a subst replacement — either a static string or a closure call.
    /// Evaluate a subst replacement — either a static string or a closure call —
    /// and apply the `:samecase`/`:samemark`/`:samespace` transforms (against
    /// `matched_text`) to the result, matching the `s///` operator.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn eval_subst_replacement_cased(
        &mut self,
        replacement_val: &Option<Value>,
        is_closure: bool,
        replacement_str: &str,
        matched_text: &str,
        captures: Option<&RegexCaptures>,
        orig_text: Option<&str>,
        transforms: SubstCaseTransforms,
    ) -> Result<String, RuntimeError> {
        let cased = |s: String| -> String { transforms.apply(&s, matched_text) };
        if !is_closure {
            // Expand $0, $1, ... capture references in string replacements
            if let Some(caps) = captures
                && !caps.positional.is_empty()
            {
                let expanded = crate::vm::vm_string_regex_ops::expand_capture_refs(
                    replacement_str,
                    &caps.positional,
                );
                return Ok(cased(expanded));
            }
            return Ok(cased(replacement_str.to_string()));
        }
        let sub_data = match replacement_val {
            Some(Value::Sub(data)) => data.clone(),
            Some(Value::WeakSub(weak)) => weak
                .upgrade()
                .ok_or_else(|| RuntimeError::new("subst closure has been garbage collected"))?,
            _ => return Ok(replacement_str.to_string()),
        };
        let mut saved = self.env.clone();
        // The replacement block may mutate variables it closes over
        // (`s/.../{ $n++ }/`, `{ $m = $/[0] }`). Remember its captured lexical
        // names so those writes can be propagated back to the caller after the
        // block runs; the blanket env restore below would otherwise discard
        // them. The match-context names injected just below (`$/`, `$_`, `$0`..,
        // `<name>`) are NOT captured lexicals and must not leak back.
        let captured_keys: Vec<crate::symbol::Symbol> = sub_data.env.keys().copied().collect();
        // Set up closure environment. Only supply captured names the live env
        // doesn't already hold: for a `:g` substitution this routine runs once
        // per match, and re-seeding from the (fixed) capture snapshot would reset
        // a closed-over counter (`{ $n++ }`) on every match. The live env carries
        // the previous match's writeback forward.
        for (k, v) in &sub_data.env {
            if self.env.get_sym(*k).is_none() {
                self.env.insert_sym(*k, v.clone());
            }
        }
        if let Some(captures) = captures {
            let match_obj = Value::make_match_object_full(
                captures.matched.clone(),
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                &captures.positional_subcaps,
                &captures.positional_quantified,
                &captures.positional_nil,
                orig_text,
            );
            self.env.insert("/".to_string(), match_obj.clone());
            self.env.insert("$_".to_string(), match_obj.clone());
            self.env.insert("_".to_string(), match_obj.clone());
            let positional_len = captures
                .positional_slots
                .len()
                .max(captures.positional.len());
            for i in 0..positional_len {
                let value = if let Some(Some((capture, _, _))) = captures.positional_slots.get(i) {
                    Value::str(capture.clone())
                } else if let Some(capture) = captures.positional.get(i) {
                    Value::str(capture.clone())
                } else {
                    Value::Nil
                };
                self.env.insert(i.to_string(), value);
            }
            if positional_len == 0 {
                self.env.insert("0".to_string(), Value::Nil);
            }
            if let Value::Instance { ref attributes, .. } = match_obj
                && let Some(Value::Hash(named_hash)) = attributes.as_map().get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
        } else {
            let match_val = Value::str(matched_text.to_string());
            self.env.insert("/".to_string(), match_val.clone());
            self.env.insert("$_".to_string(), match_val.clone());
            self.env.insert("_".to_string(), match_val);
        }
        let result = self.eval_block_value(&sub_data.body).unwrap_or(Value::Nil);
        // Propagate the block's writes to its closed-over lexicals back to the
        // caller before restoring the outer env, skipping the injected
        // match-context names (`$/`, `$_`, positional `$0`.., `<name>`).
        for k in captured_keys {
            let name = k.resolve();
            let is_match_context = name == "/"
                || name == "_"
                || name == "$_"
                || (!name.is_empty() && name.bytes().all(|b| b.is_ascii_digit()))
                || name.starts_with('<');
            if is_match_context {
                continue;
            }
            if let Some(v) = self.env.get_sym(k) {
                saved.insert_sym(k, v.clone());
                // Precise (env_dirty-independent) writeback: record the captured
                // lexical so the `.subst` call site drains it into the caller's
                // local slot via `apply_pending_rw_writeback`, like a map/grep
                // callback. Without this the write reached env but the caller's
                // slot relied on the blanket reconcile (being retired — see
                // docs/captured-outer-cell-sharing.md).
                self.pending_rw_writeback_sources.push(name);
            }
        }
        self.env = saved;
        Ok(cased(result.to_string_value()))
    }
}
