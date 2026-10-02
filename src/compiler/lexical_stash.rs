//! Lexical pseudo-stashes (`MY::`, `LEXICAL::`, `OUTER::MY::`) compiled to
//! a fixed description of one scope frame (`OpCode::GetLexicalStash`).
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
        let entries = target
            .keys()
            .map(|var_name| {
                let slot = match lex_scope::resolve_outer(&scopes, &self.local_map, var_name, depth)
                {
                    lex_scope::OuterResolution::Read { slot, .. } => slot,
                    lex_scope::OuterResolution::NotDeclared => None,
                };
                let display_name =
                    if let Some(term) = crate::runtime::term_names::term_spelling(var_name) {
                        // A sigil-less constant is listed under its spelling (#9962).
                        term.to_string()
                    } else if var_name.starts_with(['$', '@', '%', '&'])
                        || var_name.chars().next().is_some_and(|c| c.is_uppercase())
                    {
                        var_name.clone()
                    } else {
                        format!("${var_name}")
                    };
                Value::array(vec![
                    Value::str(display_name),
                    Value::str(var_name.clone()),
                    Value::int(depth as i64),
                    Value::int(slot.map_or(-1, |slot| slot as i64)),
                ])
            })
            .collect();
        let mut entries: Vec<Value> = entries;
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
        let routines = self.lexical_stash_routines(target_index);
        self.code
            .emit(OpCode::GetLexicalStash { spec_idx, routines });
        true
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
    /// besides its baked entries. A compunit or routine root (or a scope of an
    /// enclosing compilation) sees every visible routine; a nested block of
    /// this compilation holds only its own declarations, which are baked
    /// `&name` entries already, and the imports of its own `use`s.
    fn lexical_stash_routines(&self, target_index: usize) -> crate::opcode::LexicalStashRoutines {
        use crate::opcode::LexicalStashRoutines;
        if target_index <= self.unit_root_index() {
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
