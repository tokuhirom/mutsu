//! Lexical pseudo-stashes (`MY::`, `LEXICAL::`, `OUTER::MY::`) compiled to
//! a fixed description of one scope frame — or, for `LEXICAL::`, of that
//! frame and every frame enclosing it (`OpCode::GetLexicalStash`).
use super::*;
use crate::value::V6E_PSEUDO_STASH;

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
        // A bare `OUTER::` is the pad one scope out, `OUTER::MY::`.
        if remaining == "OUTER" {
            depth += 1;
            remaining = "MY";
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
                // The `self` term is no lexical; `$self` is `LEX_SELF`
                // (ADR-0061), which displays as `$self`.
                if var_name == "self" {
                    continue;
                }
                if !seen.insert(var_name.as_str()) {
                    continue;
                }
                let slot =
                    match lex_scope::resolve_outer(&scopes, &self.local_map, var_name, frame_depth)
                    {
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
        // The variable the statement being compiled declares is in the pad
        // already (#15607).
        if depth == 0
            && !is_lexical
            && let Some(name) = &self.decl_in_flight
            && !seen.contains(name.as_str())
        {
            entries.push(Value::array(vec![
                Value::str(Self::lexical_stash_display_name(name)),
                Value::str(name.clone()),
                Value::int(0),
                Value::int(-1),
            ]));
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
        // A unit that exports a routine holds its `EXPORT` package in its
        // root scope (#13854).
        if self.unit_exports_routine
            && target_index == self.enclosing_scopes.len() + self.unit_root_scope
            && !seen.contains("EXPORT")
        {
            entries.push(Value::array(vec![
                Value::str("EXPORT".to_string()),
                Value::str("EXPORT".to_string()),
                Value::int(depth as i64),
                Value::int(crate::opcode::LEXICAL_STASH_EXPORT_SLOT),
            ]));
        }
        // Every routine and every compilation unit's mainline declares `$/`,
        // `$!` and `$¢` in its outermost frame (rakudo's `MY::` of a sub,
        // method or mainline lists them; a bare block or pointy block does
        // not). They are read by name, so the frame's current match/error
        // shows, `Nil` until something sets it (#17674).
        let root_index = self.enclosing_scopes.len() + self.unit_root_scope;
        let declares_specials = self.is_routine || !self.in_lexical_scope;
        if declares_specials && (outermost..=target_index).contains(&root_index) {
            let root_depth = depth + (target_index - root_index);
            for (display, name) in [("$!", "!"), ("$/", "/"), ("$¢", "¢")] {
                if seen.contains(name) || seen.contains(display) {
                    continue;
                }
                entries.push(Value::array(vec![
                    Value::str(display.to_string()),
                    Value::str(name.to_string()),
                    Value::int(root_depth as i64),
                    Value::int(-1),
                ]));
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

    /// Under 6.e, `MY::` is the innermost pad and nothing else: `$MY::x` or
    /// `MY::Name` naming a symbol that pad does not declare is a `MY::<key>`
    /// read, whose miss is an `X::NoSuchSymbol` Failure (rakudo 2026.11; 6.c
    /// and 6.d still find an enclosing scope's `$x`). Emits that read and
    /// returns true when `name` is such a spelling; `sigil` is the variable's
    /// sigil (empty for a bare word).
    // Cost: O(|name|) at compile time.
    pub(crate) fn emit_strict_my_read(&mut self, sigil: &str, name: &str) -> bool {
        let Some(bare) = name.strip_prefix("MY::") else {
            return false;
        };
        if bare.is_empty()
            || bare.contains("::")
            || !super::compile_inputs::current_language_version_starts_with("6.e")
        {
            return false;
        }
        // The pad's own key for the symbol: a scalar's is its bare name.
        let pad_key = if sigil == "$" {
            bare.to_string()
        } else {
            format!("{sigil}{bare}")
        };
        if self.innermost_pad_declares(&pad_key, sigil.is_empty()) {
            return false;
        }
        self.compile_expr(&Expr::Index {
            target: Box::new(Expr::PseudoStash("MY::".to_string())),
            index: Box::new(Expr::Literal(Value::str(format!("{sigil}{bare}")))),
            is_positional: false,
            spelling: Default::default(),
        });
        true
    }

    /// Whether the innermost scope declares `pad_key` (a scalar's bare name,
    /// another variable's sigiled one), or, for a `type` spelling, the type.
    // Cost: O(n), n = names the innermost scope declares.
    fn innermost_pad_declares(&self, pad_key: &str, type_name: bool) -> bool {
        let Some(frame) = self.local_scopes.last().or(self.enclosing_scopes.last()) else {
            return false;
        };
        frame.all_declared_names().any(|n| n == pad_key)
            || (type_name && frame.declared_type(pad_key).is_some())
    }

    /// `$MY::x` as an assignment or bind target names the innermost scope's
    /// own `$x` when that scope declares it, an `our $x` alias included:
    /// `$MY::a18 := $x` rebinds the lexical alias, as `$a18 := $x` does.
    /// Returns the plain spelling to store through, or `None`.
    // Cost: O(n), as `innermost_pad_declares`.
    /// `$LEXICAL::x` names the nearest lexical `$x`, the one the plain `$x`
    /// sees, so it is stored through that spelling too.
    pub(crate) fn my_pad_store_name<'a>(&self, name: &'a str) -> Option<&'a str> {
        if let Some(bare) = name.strip_prefix("LEXICAL::") {
            return (!bare.is_empty() && !bare.contains("::") && self.lexical_chain_declares(bare))
                .then_some(bare);
        }
        let bare = name.strip_prefix("MY::")?;
        (!bare.is_empty() && !bare.contains("::") && self.innermost_pad_declares(bare, false))
            .then_some(bare)
    }

    /// Whether any scope of this compilation's chain declares `pad_key`.
    // Cost: O(s * n), s = scopes, n = names each declares.
    fn lexical_chain_declares(&self, pad_key: &str) -> bool {
        self.enclosing_scopes
            .iter()
            .chain(self.local_scopes.iter())
            .any(|frame| frame.all_declared_names().any(|n| n == pad_key))
    }

    /// `$LEXICAL::x` read where a scope of this compilation declares `$x`:
    /// the plain `$x` read, the nearest lexical (#18742). Returns the plain
    /// spelling, or `None` to resolve the qualified name at run time.
    // Cost: O(s * n), as `lexical_chain_declares`.
    /// `$LEXICAL::A1::pies` names the member of the package the plain `A1`
    /// finds, the lexical one first, so it reads as `$A1::pies`.
    pub(crate) fn lexical_read_name<'a>(&self, name: &'a str) -> Option<&'a str> {
        let bare = name.strip_prefix("LEXICAL::")?;
        if bare.is_empty() {
            return None;
        }
        if let Some((package, _)) = bare.split_once("::") {
            return (self.lexical_types_in_scope.contains(package)
                || self.lexical_chain_declares(package))
            .then_some(bare);
        }
        self.lexical_chain_declares(bare).then_some(bare)
    }

    /// The variable spelling (`x`, `@a`, `*k`) a `$x`/`@a`/`$*k` stash key
    /// names, when it is a plain variable.
    // Cost: O(|key|).
    fn stash_key_var_name(key: &str) -> Option<String> {
        let sigil = key
            .chars()
            .next()
            .filter(|c| matches!(c, '$' | '@' | '%'))?;
        let rest = &key[1..];
        let ident = rest.strip_prefix('*').unwrap_or(rest);
        if !ident.starts_with(|c: char| c.is_alphabetic() || c == '_') || ident.contains("::") {
            return None;
        }
        Some(if sigil == '$' {
            rest.to_string()
        } else {
            key.to_string()
        })
    }

    /// `LEXICAL::<$x> := v` and the root `::<$x> := v` bind the variable the
    /// plain `$x` names (the nearest lexical, or the dynamic `$*x`), as
    /// `$x := v` does; the stash itself is only a view of the pads. Emits
    /// that bind and returns true when `target`/`index`/`value` spell one.
    pub(crate) fn emit_lexical_stash_bind(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> bool {
        let (Expr::PseudoStash(stash), Expr::Literal(lit), Expr::IndexBindValue(rhs)) =
            (target, index, value)
        else {
            return false;
        };
        if !matches!(stash.as_str(), "::" | "LEXICAL::") {
            return false;
        }
        let Some(name) = lit.as_str().and_then(Self::stash_key_var_name) else {
            return false;
        };
        if !name.starts_with('*') && !self.lexical_chain_declares(&name) {
            return false;
        }
        self.compile_expr(&Expr::AssignExpr {
            name,
            expr: rhs.clone(),
            is_bind: true,
        });
        true
    }

    /// `CORE::v6c::Name` (also `v6d`, `v6e`): the setting of one language
    /// revision. Its `CORE-SETTING-REV` is that revision's letter, and its
    /// `PseudoStash` is that revision's class: 6.e redefines it (a miss
    /// fails), while 6.d has none of its own and so names 6.c's. Any other
    /// name is the core one. Emits the name and returns true for such a
    /// spelling.
    // Cost: O(|name|) at compile time.
    pub(crate) fn emit_core_revision_name(&mut self, name: &str) -> bool {
        let Some(rest) = name.strip_prefix("CORE::v6") else {
            return false;
        };
        let mut chars = rest.chars();
        let (Some(rev @ ('c' | 'd' | 'e')), Some(':'), Some(':')) =
            (chars.next(), chars.next(), chars.next())
        else {
            return false;
        };
        let member = &rest[3..];
        if member.is_empty() || member.contains("::") {
            return false;
        }
        let value = match member {
            "CORE-SETTING-REV" => Value::str(rev.to_string()),
            "PseudoStash" if rev == 'e' => Value::package(Symbol::intern(V6E_PSEUDO_STASH)),
            _ => {
                self.compile_expr(&Expr::BareWord(format!("CORE::{member}")));
                return true;
            }
        };
        let idx = self.code.add_constant(value);
        self.code.emit(OpCode::LoadConst(idx));
        true
    }

    /// Emit `CORE::<key>`: the setting's entry under `key` (a `&name`
    /// routine), or Nil.
    // Cost: one keyed pseudo-stash read.
    pub(crate) fn emit_core_stash_read(&mut self, key: &str) {
        self.compile_subscript_index(&Expr::Literal(Value::str(key.to_string())));
        let name_idx = self.code.add_constant(Value::str("CORE::".to_string()));
        self.code.emit(OpCode::GetPseudoStashKeyed(name_idx));
    }

    /// With the value of a lexical lookup of the routine `key` (`&name`) on
    /// the stack, replace a Nil by the setting's routine of that name: the
    /// lexical scopes `LEXICAL::` and `OUTERS::` see end in CORE.
    // Cost: O(1) when found; otherwise one keyed pseudo-stash read.
    pub(crate) fn emit_core_routine_fallback(&mut self, key: &str) {
        self.code.emit(OpCode::Dup);
        let found = self.code.emit(OpCode::JumpIfNotNil(0));
        self.code.emit(OpCode::Pop);
        self.code.emit(OpCode::Pop);
        self.emit_core_stash_read(key);
        let end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(found);
        self.code.emit(OpCode::Pop);
        self.code.patch_jump(end);
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

    /// Note that the innermost scope frame holds a statement that opens a
    /// run-time import scope (`Compiler::opens_import_scope`: a `use`, `need`,
    /// `import`, `no` or `require`) of its own (see
    /// [`Self::import_scope_levels`]).
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
