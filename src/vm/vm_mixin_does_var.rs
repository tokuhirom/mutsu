use super::*;

impl Interpreter {
    // Cost: role composition plus the recorded-write drain, as `exec_does_op`.
    pub(super) fn exec_does_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
        is_bareword: bool,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A `submethod BUILD`/`TWEAK` run by the mixin can mutate a captured-outer
        // caller lexical (`my $n=0; role R { submethod TWEAK { $n++ } }; $x does
        // R`); see exec_does_op for why no whole-frame sync/snapshot is needed.
        let attr_trait = Self::attribute_trait_target(&left);
        let composition_left = attr_trait
            .as_ref()
            .and_then(|key| {
                self.registry()
                    .class_attribute_trait_objects
                    .get(key)
                    .cloned()
            })
            .unwrap_or(left);
        let updated = self.vm_does_values(composition_left, right)?;
        if let Some((owner, attr_name)) = attr_trait
            && crate::value::attribute_class::is_composed_meta_object(&updated)
        {
            self.registry_mut()
                .class_attribute_trait_objects
                .insert((owner, attr_name), updated.clone());
        }
        // BUILD submethods may have modified closure variables.
        self.apply_pending_caller_var_writeback(code);
        let name = Self::const_str(code, name_idx).to_string();
        // `Apple does R` on an ENUM KEY mutates the key's own binding, which lives
        // in the enum-key namespace, not under the plain `env` key (#7914 — that
        // key belongs to `$Apple`). Only a bareword target can name an enum key;
        // `$Apple does R` is a scalar rebind and takes the ordinary road below.
        if is_bareword && self.enum_bare_value(&name).is_some() {
            self.insert_enum_bare_value(&name, updated.clone());
            self.stack.push(updated);
            return Ok(());
        }
        // The same by-name store an assignment uses, NOT a raw `env.insert`:
        // `set_env_with_main_alias` is what redirects a compunit lexical to its
        // own cell, logs the write into the active carrier so the carrier-return
        // writeback reconciles it into the caller's slot, and maintains the
        // `Main::`/dynamic aliases. `$x does R` publishes the composed object
        // through that binding — a raw insert landed in a
        // tier the caller never read, and `lives-ok { $a does role { has $.cool } }`
        // left `$a` un-mixed (roast/S14-roles/anonymous.t).
        // Composition mutates the held object; it must not replace the
        // binding cell with the resulting value in the local mirror.
        let stored = self
            .read_local_slot_or_name(code, slot, &name)
            .or_else(|| self.get_env_with_main_alias(&name))
            .filter(|binding| matches!(binding.view(), ValueView::ContainerRef(_)))
            .map_or_else(
                || updated.clone(),
                |binding| {
                    if let ValueView::ContainerRef(cell) = binding.view() {
                        cell.store(updated.clone());
                    }
                    binding
                },
            );
        self.set_env_with_main_alias(&name, stored.clone());
        // §1.5: mirror into the compiler-baked local slot when known, else by name.
        self.write_local_slot_or_name(code, slot, &name, stored);
        // Capture Mixin value for trait_mod writeback: when `$r does Role`
        // runs inside a trait_mod:<is>, the Mixin needs to propagate back
        // to the outer scope's `&name` variable.
        if crate::value::attribute_class::is_composed_meta_object(&updated) && self.trait_mod_writeback_key.is_some()
        {
            self.trait_mod_writeback_value = Some(updated.clone());
            // See `trait_mod_attr_writeback_value`'s doc comment: captured
            // separately so a LATER unrelated `does` in the same handler
            // (e.g. `$class.HOW does OtherRole`) cannot clobber the value
            // this named variable's own mixin needs cached.
            if Self::how_target_from_value(&updated).is_none() {
                self.trait_mod_attr_writeback_value = Some(updated.clone());
            }
        }
        self.stack.push(updated);
        Ok(())
    }
}
