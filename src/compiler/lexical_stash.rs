//! Lexical pseudo-stashes (`MY::`, `LEXICAL::`, `OUTER::MY::`) compiled to
//! a fixed description of one scope frame — or, for `LEXICAL::`, of that
//! frame and every frame enclosing it (`OpCode::GetLexicalStash`).
use super::*;

impl Compiler {
    /// Emit a pseudo-stash for exactly one lexical frame when `name` is a
    /// literal `MY::`/`LEXICAL::` spelling, optionally preceded by one or more
    /// `OUTER::` prefixes. The ordinary runtime pseudo-stash path is backed by
    /// the flattened environment, which is intentionally broader than one
    /// lexical frame and therefore makes `MY::` leak enclosing variables.
    /// Whether `name` spells a lexical pseudo-stash (`MY::`/`LEXICAL::`,
    /// optionally behind `OUTER::` prefixes) — the stashes
    /// [`Self::emit_lexical_stash`] may compile to a fixed scope description.
    pub(crate) fn is_lexical_stash_name(name: &str) -> bool {
        let Some(mut remaining) = name.strip_suffix("::") else {
            return false;
        };
        while let Some(rest) = remaining.strip_prefix("OUTER::") {
            remaining = rest;
        }
        matches!(remaining, "MY" | "LEXICAL")
    }

    pub(crate) fn emit_lexical_stash(&mut self, name: &str) -> bool {
        let Some(stash_name) = name.strip_suffix("::") else {
            return false;
        };
        let mut remaining = stash_name;
        let mut depth = 0usize;
        while let Some(rest) = remaining.strip_prefix("OUTER::") {
            depth += 1;
            remaining = rest;
        }
        if !matches!(remaining, "MY" | "LEXICAL") {
            return false;
        }

        let scopes = self.full_scope_chain();
        let Some(target_index) = scopes.len().checked_sub(depth + 1) else {
            return false;
        };
        let target = &scopes[target_index];
        let is_lexical = remaining == "LEXICAL";
        // `MY::` is the target frame alone. `LEXICAL::` is every lexical
        // visible from it (#10858): the target frame and each frame enclosing
        // it, an inner declaration shadowing an outer one of the same name.
        let outermost = if is_lexical { 0 } else { target_index };
        let mut seen: HashSet<&str> = HashSet::new();
        let mut entries: Vec<Value> = Vec::new();
        for frame_index in (outermost..=target_index).rev() {
            let frame_depth = depth + (target_index - frame_index);
            for var_name in scopes[frame_index].keys() {
                if !seen.insert(var_name.as_str()) {
                    continue;
                }
                let slot = match lex_scope::resolve_outer(
                    &scopes,
                    &self.local_map,
                    var_name,
                    frame_depth,
                ) {
                    lex_scope::OuterResolution::Read { slot, .. } => slot,
                    lex_scope::OuterResolution::NotDeclared => None,
                };
                entries.push(Value::array(vec![
                    Value::str(Self::lexical_stash_display_name(var_name)),
                    Value::str(var_name.clone()),
                    Value::int(frame_depth as i64),
                    Value::int(slot.map_or(-1, |slot| slot as i64)),
                ]));
            }
        }
        if let Some(local_index) = target_index.checked_sub(self.enclosing_scopes.len()) {
            let level = local_index + 1;
            for &(_, name) in self
                .scope_routine_decls
                .iter()
                .filter(|&&(decl_level, _)| decl_level == level)
            {
                let key = format!("&{}", name.resolve());
                if !target.contains_key(&key) {
                    entries.push(Value::array(vec![
                        Value::str(key.clone()),
                        Value::str(key),
                        Value::int(depth as i64),
                        Value::int(crate::opcode::LEXICAL_STASH_ROUTINE_SLOT),
                    ]));
                }
            }
        }
        let spec_idx = self.code.add_constant(Value::array(entries));
        // `LEXICAL::` is every lexical visible from the frame, so its routines
        // are all the visible ones; only `MY::` is narrowed to the frame's own.
        let routines = if is_lexical {
            crate::opcode::LexicalStashRoutines::All
        } else {
            self.lexical_stash_routines(target_index)
        };
        self.code
            .emit(OpCode::GetLexicalStash { spec_idx, routines });
        true
    }

    /// The key a pseudo-stash lists the scope-frame entry `var_name` under.
    fn lexical_stash_display_name(var_name: &str) -> String {
        if let Some(term) = crate::runtime::term_names::term_spelling(var_name) {
            // A sigil-less constant is listed under its spelling (#9962).
            term.to_string()
        } else if var_name.starts_with(['$', '@', '%', '&'])
            || var_name.chars().next().is_some_and(|c| c.is_uppercase())
        {
            var_name.to_string()
        } else {
            format!("${var_name}")
        }
    }

    /// Note that the innermost scope frame declares the routine `name`.
    pub(crate) fn note_scope_routine(&mut self, name: Symbol) {
        let level = self.local_scopes.len();
        if !self.scope_routine_decls.contains(&(level, name)) {
            self.scope_routine_decls.push((level, name));
        }
    }

    /// Note that the innermost scope frame holds a `use`/`import`/`no` of its
    /// own (see [`Self::import_scope_levels`]).
    pub(crate) fn note_import_in_scope(&mut self) {
        let level = self.local_scopes.len();
        if self.import_scope_levels.last() != Some(&level) {
            self.import_scope_levels.push(level);
        }
    }

    /// Which routines the pad at `target_index` of the full scope chain holds
    /// besides its baked entries. A compunit's own root (or a scope of an
    /// enclosing compilation) sees every visible routine. Any other frame of
    /// this compilation — a nested block, or the top-level pad of a routine or
    /// closure body ([`Self::in_lexical_scope`], #10849) — holds only its own
    /// declarations, which are baked `&name` entries already, and the imports
    /// of its own `use`s (a body with a `use` runs inside an `ImportScope`).
    fn lexical_stash_routines(&self, target_index: usize) -> crate::opcode::LexicalStashRoutines {
        use crate::opcode::LexicalStashRoutines;
        let unit_root = self.unit_root_index();
        if target_index < unit_root || (target_index == unit_root && !self.in_lexical_scope) {
            return LexicalStashRoutines::All;
        }
        let level = target_index - self.enclosing_scopes.len() + 1;
        if !self.import_scope_levels.contains(&level) {
            return LexicalStashRoutines::None;
        }
        let skip = self
            .import_scope_levels
            .iter()
            .filter(|&&deeper| deeper > level)
            .count() as u32;
        LexicalStashRoutines::OwnImports { skip }
    }
}
