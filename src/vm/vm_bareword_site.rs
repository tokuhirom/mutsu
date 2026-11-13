//! `OpCode::GetBareWord`, with the per-site type-object memo (ADR-0121 D3).
//!
//! A bareword term is resolved through `push_bare_word_value`'s whole chain
//! on every execution. When that chain answers the type object of the name's
//! own spelling (`P` → `P`), the answer depends only on what is declared, so
//! the chunk remembers it for one registry write generation
//! ([`BarewordSiteCaches`](crate::value::BarewordSiteCaches)) and a later
//! execution under the same generation pushes it without resolving.
//!
//! Three more things feed the chain ahead of its type branch, and the memo is
//! sound against each:
//!
//! - **A same-named `env` binding.** A type capture (`sub f(::T $x) { T }`),
//!   a role's type parameter, or a sigil-stripped `my $P` holding a package
//!   are all read from `env` under the bare name, and rebinding them writes
//!   no registry. So a site is only remembered, and a memo only used, while
//!   `env` binds nothing under the name, or binds that very type object (as
//!   a class declaration does for its own name) — and binds no sigil-less
//!   constant under the name's term key, which outranks the type (#9963).
//!   Those two probes are the one re-check a hit pays.
//! - **An enum member with the same spelling.** Declaring an enum registers
//!   its type, which bumps the generation.
//! - **Spellings the chain rewrites from bindings.** A parameterised name
//!   (`Box[T]`) or a smiley on a bound type parameter (`T:D`) is substituted
//!   from `env` under a *different* key than the name, so such names are
//!   never remembered.
//!
//! The generation is per interpreter, and every interpreter's counter starts
//! in a range of its own (`Interpreter::fresh_registry_write_gen`), so a memo
//! a thread wrote into a shared chunk is never read as current by another
//! interpreter whose registry snapshot differs.

use super::*;

impl Interpreter {
    // Cost: O(1) on a memo hit (one lock, two `env` probes, one provenance
    // probe; a module-published name adds the O(d) merge check, d = EVAL
    // parent-chain depth); a miss costs one bareword resolution, O(p*|name|),
    // p = packages on the bare-name search path.
    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let generation = self.registry_write_generation();
        let sites = code.constants.len();
        let idx = name_idx as usize;
        if let Some(value) = code.bareword_sites.cached(sites, idx, generation) {
            let name_sym = code.const_sym(name_idx);
            let env_ok = match value.view() {
                ValueView::Package(sym) => {
                    self.env_leaves_type_name_as(name_sym, sym)
                        && self.lexical_memo_still_in_scope(code, name_sym, sym)
                }
                ValueView::Enum { .. } if !crate::qualified::is_qualified(name_sym) => {
                    (chunk_declares_no_types(code)
                        || self.env_binds_name_to_value(name_sym, &value))
                        && self.env_answers_memo(name_sym, &value)
                }
                _ => self.env_answers_memo(name_sym, &value),
            };
            if env_ok && self.bare_name_visible_here(name_sym) {
                // The sentinel is pushed as a fresh handle, not the shared
                // memo's: every thread running this chunk would otherwise
                // bump one refcount.
                self.stack.push(if value.is_iteration_end() {
                    Value::iteration_end()
                } else {
                    value
                });
                return Ok(());
            }
        }
        let name = Self::const_str(code, name_idx);
        // A `C[T]` spelling of a class with its own `^parameterize` (the
        // desugared `my Pointer[uint16] $p .= new`) is the type object that
        // meta-method builds, memoized per spelling.
        if name.contains('[')
            && let Some(ty) = self.meta_parameterized_type(name)
        {
            self.stack.push(ty);
            return Ok(());
        }
        // A type the main program declared, read from a REPL session's unit
        // (#17642).
        if self.main_type_hidden_in_code(code, code.const_sym(name_idx)) {
            return self.push_hidden_type_name(name, compiled_fns);
        }
        if let Some(member) = self.unit_lexical_qualified_member(code, code.const_sym(name_idx)) {
            self.stack.push(member);
            return Ok(());
        }
        let foreign_unit = self.code_is_loaded_module_code(code);
        self.push_bare_word_value_in(code.const_sym(name_idx), compiled_fns, false, foreign_unit)?;
        let unit_lexical = self.resolve_unit_lexical_type(code, name_idx);
        let name_sym = code.const_sym(name_idx);
        // The type object of the name's own spelling, or the unit-level
        // lexical (`my`) type of that spelling under its ADR-0047 storage
        // name: a module's `my class` read from the module's own subs
        // (ParaSeq's `ParaStats`, `BufferIterator`, ...) resolved through the
        // whole chain on every execution because only the first form was
        // remembered (#17337). The memo is shared by every site of the chunk
        // spelling the name, and a block-scoped lexical type is not the answer
        // outside its block, so a non-unit lexical type is remembered only
        // while `env` binds the name to that very type (the binding its
        // declaration installs, which leaves with its block), and every hit
        // re-checks that binding (`lexical_memo_still_in_scope`). That is the
        // shape of a role or class body's `my class` read from its methods
        // (AttrX::Mooish's `AttrProxy`, `NO-VALUE-YET`).
        let memoizable = match self.stack.last().map(Value::view) {
            Some(ValueView::Package(sym)) => {
                (sym == name
                    || (is_lexical_storage_of(sym, name)
                        && (unit_lexical
                            || self.env_binds_name_to(name_sym, sym)
                            || chunk_declares_no_types(code)
                            || self.is_unit_lexical_type_of(code, name_sym, sym))))
                    && bareword_memoizable(name)
                    && self.env_leaves_type_name_as(name_sym, sym)
            }
            // The `IterationEnd` sentinel, which the chain answers only after
            // every declaration of the name had first refusal: a `sub`,
            // `constant` or type of that name is a registry write or an `env`
            // binding, which the generation and the re-check cover. It is read
            // on every `pull-one` of a user iterator.
            Some(ValueView::Instance { .. })
                if name == "IterationEnd"
                    && self.stack.last().is_some_and(Value::is_iteration_end) =>
            {
                self.stack
                    .last()
                    .is_some_and(|v| self.env_answers_memo(name_sym, v))
            }
            // A package-qualified enum key (`Order::Less`) names one
            // immutable value of a declared enum: the registry decides it.
            Some(ValueView::Enum { enum_type, key, .. }) => {
                let qualified_key = crate::qualified::is_qualified(name_sym)
                    && crate::qualified::split_qualified(name_sym) == Some((enum_type, key));
                // A bare key (`pvmAuto`) of an enum is remembered only while
                // `env` binds it to that very value: the binding its enum's
                // declaration installs, which a `my enum` scopes to its block
                // (AttrX::Mooish's role-body `PvtMode`). Every hit re-checks it.
                // The same holds in a chunk that declares no type of its own
                // (`chunk_declares_no_types`): every site of it resolves the
                // bare key alike.
                let bound_bare_key = !crate::qualified::is_qualified(name_sym)
                    && key == name_sym
                    && (chunk_declares_no_types(code)
                        || self
                            .stack
                            .last()
                            .is_some_and(|v| self.env_binds_name_to_value(name_sym, v)));
                (qualified_key || bound_bare_key)
                    && self
                        .stack
                        .last()
                        .is_some_and(|v| self.env_answers_memo(name_sym, v))
            }
            _ => false,
        };
        if memoizable && let Some(value) = self.stack.last().cloned() {
            code.bareword_sites.remember(sites, idx, generation, value);
        }
        Ok(())
    }

    /// Whether `env` lets the bareword `name` keep answering `value`, the
    /// site's memo: it binds nothing under the name or its term key, or
    /// binds that very type object or enum value there (an enum declaration
    /// binds its type and its `Type::Key` values so). Any other binding may
    /// be what the resolution chain answers, and it changes without a
    /// registry write.
    ///
    /// A name no env tier has ever held (the core `Order::Same`, bound only in
    /// the base tier) is answered from the base tier without walking the
    /// chain: the walk can only answer that entry or nothing, and the check
    /// holds for nothing, so holding for the entry is the stronger test.
    // Cost: O(1), two `env` probes (a base-only name: O(t) pointer steps, t =
    // env tiers, instead of a probe per tier).
    fn env_answers_memo(&self, name: Symbol, value: &Value) -> bool {
        let holds = |bound: Option<&Value>| match bound {
            None => true,
            Some(bound) => {
                matches!(bound.view(), ValueView::Package(p) if p == name)
                    || same_memo_value(bound, value)
            }
        };
        let env = self.env();
        let bound = match env.base_only_get(name) {
            Some(base) => base,
            None => env.get_sym(name),
        };
        let term = crate::runtime::term_names::term_key_sym(name);
        let term_bound = match env.base_only_get(term) {
            Some(base) => base,
            None => env.get_sym(term),
        };
        holds(bound) && holds(term_bound)
    }

    /// `OpCode::GetTypeCapture`: push the live binding of the type capture
    /// the operand names, when `env` holds one under its marker. `false`
    /// leaves the stack untouched for the `GetBareWord` resolution.
    // Cost: O(d), d = env tiers walked (the marker probe and the value probe).
    pub(super) fn exec_get_type_capture_op(&mut self, code: &CompiledCode, name_idx: u32) -> bool {
        let name = code.const_sym(name_idx);
        if !name.with_str(|n| self.has_type_capture_binding(n)) {
            return false;
        }
        let Some(value) = self.env().get_sym(name).cloned() else {
            return false;
        };
        self.stack.push(value);
        true
    }

    /// A bare word that resolved to nothing but its own name, read by a loaded
    /// module's code, is the `my` class / role that module's compunit declared,
    /// when it has one: the load may have bound it in a tier the code's static
    /// link skips (a `use` under an `EVAL`'s import region, #17129).
    // Cost: O(1).
    /// Returns whether it replaced the answer.
    /// Whether `storage` is the unit-level lexical (`my`) type `code`'s own
    /// compunit declared under the spelling `name` -- the answer the bareword
    /// chain gives for it everywhere in that unit, whichever branch of the
    /// chain produced it.
    // Cost: O(1), one hash probe.
    fn is_unit_lexical_type_of(&self, code: &CompiledCode, name: Symbol, storage: Symbol) -> bool {
        code.source_file
            .is_some_and(|unit| self.unit_lexical_type(unit, name) == Some(storage))
    }

    /// `Enums::Status` in a module routine where `Enums` is the module's own
    /// lexical package (`my package Enums { our enum Status ... }`), run from
    /// another compunit: the routine's `env` no longer binds `Enums`, so the
    /// head resolves through the lexical types `code`'s own unit declared, as
    /// rakudo resolves it in the routine's lexical scope at compile time. Only
    /// code of a loaded module has unit lexical types, so a program naming a
    /// module's lexical package still fails, as in rakudo (KeyNub::LicDongle).
    // Cost: O(1) for an unqualified name or an env-bound head; otherwise one
    // hash probe plus the member lookup.
    fn unit_lexical_qualified_member(&self, code: &CompiledCode, name: Symbol) -> Option<Value> {
        let unit = code.source_file?;
        if !crate::qualified::is_qualified(name) {
            return None;
        }
        let segments = crate::qualified::segments(name);
        let (&head, rest) = segments.split_first()?;
        if rest.is_empty() || self.type_name_binding(head.as_str()).is_some() {
            return None;
        }
        let storage = self.unit_lexical_type(unit, head)?;
        let real = rest
            .iter()
            .fold(storage, |pkg, &segment| crate::qualified::qualified(pkg, segment));
        self.lexical_package_member(real.as_str())
    }

    fn resolve_unit_lexical_type(&mut self, code: &CompiledCode, name_idx: u32) -> bool {
        let Some(unit) = code.source_file else {
            return false;
        };
        let name = code.const_sym(name_idx);
        if !matches!(self.stack.last().map(Value::view), Some(ValueView::Str(s)) if s.as_str() == name.as_str())
        {
            return false;
        }
        if let Some(storage) = self.unit_lexical_type(unit, name) {
            self.stack.pop();
            self.stack.push(Value::package(storage));
            return true;
        }
        false
    }

    /// `OpCode::GetBareWordOverScalar`: the bare word resolution minus the
    /// scalar `env` entry the name shares (see the opcode). No type-object
    /// memo — the name has a live `env` binding, which the memo refuses anyway.
    // Cost: O(p*|name|), p = packages on the bare-name search path.
    pub(super) fn exec_get_bare_word_over_scalar_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        if self.main_type_hidden_in_code(code, code.const_sym(name_idx)) {
            return self.push_hidden_type_name(Self::const_str(code, name_idx), compiled_fns);
        }
        let foreign_unit = self.code_is_loaded_module_code(code);
        self.push_bare_word_value_in(code.const_sym(name_idx), compiled_fns, true, foreign_unit)
    }

    /// Whether `env` leaves the bareword `name` to name its own type object:
    /// nothing is bound under the name, or what is bound is that type object
    /// (a class declaration binds its own name so; so does a type capture
    /// bound to the same-named class). Any other binding may be what the
    /// resolution chain answers, and it changes without a registry write.
    /// Nor may a sigil-less constant of that spelling be live in `env`: it
    /// shadows the type (#9963), and a block-scoped `use` installs one
    /// without a registry write either.
    // Cost: O(1), two `env` probes.
    pub(crate) fn env_leaves_type_name(&self, name: Symbol) -> bool {
        self.env_leaves_type_name_as(name, name)
    }

    /// [`Self::env_leaves_type_name`] for a bareword `name` that resolves to
    /// the type object `resolved`: `name` itself, or the storage name of the
    /// lexical type of that spelling. A binding of `name` to `resolved` (the
    /// declaration's own) leaves it; any other binding does not.
    // Cost: O(1), two `env` probes.
    pub(crate) fn env_leaves_type_name_as(&self, name: Symbol, resolved: Symbol) -> bool {
        let leaves = match self.env().get_sym(name) {
            None => true,
            Some(v) => {
                matches!(v.view(), ValueView::Package(p) if p == name || p == resolved)
            }
        };
        leaves
            && self
                .env()
                .get_sym(crate::runtime::term_names::term_key_sym(name))
                .is_none()
    }

    /// Whether `env` binds `name` to exactly the type object `sym`.
    // Cost: O(1), one `env` probe.
    fn env_binds_name_to(&self, name: Symbol, sym: Symbol) -> bool {
        matches!(
            self.env().get_sym(name).map(Value::view),
            Some(ValueView::Package(p)) if p == sym
        )
    }

    /// Whether `env` binds `name` to the very memoizable answer `value` (the
    /// same type object, or the same enum value by type and key).
    // Cost: O(1), one `env` probe.
    fn env_binds_name_to_value(&self, name: Symbol, value: &Value) -> bool {
        self.env()
            .get_sym(name)
            .is_some_and(|bound| same_memo_value(bound, value))
    }

    /// A memoized bareword answer that is a lexical (`my`) type, under its
    /// ADR-0047 storage name, still answers here only while it is in scope:
    /// `env` binds the name to it (the declaration's binding), or it is the
    /// unit-level lexical type of the name, which every site of the unit's
    /// chunks sees. Any other memo value passes.
    // Cost: O(1) for a non-lexical answer or a bound one; else one unit
    // lexical-type probe.
    fn lexical_memo_still_in_scope(&self, code: &CompiledCode, name: Symbol, sym: Symbol) -> bool {
        if sym == name || !is_lexical_storage_of(sym, name.as_str()) {
            return true;
        }
        self.env_binds_name_to(name, sym)
            || chunk_declares_no_types(code)
            || self.is_unit_lexical_type_of(code, name, sym)
    }

    /// Whether `env` binds the bareword `name` to the type object a type
    /// declaration of that spelling installed: the type itself, or a lexical
    /// (`my`) type registered under its ADR-0047 storage name
    /// (`RIS\u{0}<decl-id>`, possibly package-qualified). A `$`-scalar of the
    /// same spelling holding some other type object does not count.
    // Cost: O(|p|), p = the bound package name (the unqualified part is memoized).
    pub(crate) fn env_binds_declared_type(&self, name: &str) -> bool {
        let Some(v) = self.env().get(name) else {
            return false;
        };
        let ValueView::Package(p) = v.view() else {
            return false;
        };
        crate::qualified::unqualified_part(p)
            .with_str(|short| short.split('\u{0}').next() == Some(name))
    }

    /// The storage key of a `my role R` declared inside an enclosing package
    /// (`unit class M; my role R {...}` registers `M::R\u{0}<decl-id>`), found
    /// by walking the bare-name package chain innermost first. A sub of `M`
    /// called from another unit has no `env` binding for `R`, and the plain
    /// `M::R` probe misses the mangled key.
    // Cost: O(p*t), p = packages on the bare-name search path, t = registered types.
    fn package_lexical_role_key(&self, name: &str) -> Option<String> {
        self.bare_name_packages().into_iter().find_map(|pkg| {
            if crate::qualified::is_global_name(&pkg) {
                return None;
            }
            let qualified =
                crate::qualified::qualified_text(pkg.split("::&").next().unwrap_or(&pkg), name);
            self.resolve_lexical_type_key(qualified.as_str())
                .filter(|key| self.has_role(key))
        })
    }

    /// The role a `does`/`but` initializer `R(v)` names
    /// ([`OpCode::JumpIfNotRole`]), if `name` names one, resolved as a bare
    /// type name in call position is (a lexical `my role R`, or `M::R` from
    /// inside package `M`; see [`Interpreter::resolve_bare_type_name`]).
    // Cost: O(p*|name|), p = packages on the bare-name search path.
    pub(crate) fn mixin_role_sym(&self, name: Symbol) -> Option<Symbol> {
        let spelled = name.resolve();
        match self.resolve_bare_type_name(&spelled) {
            Some(resolved) => self.has_role(&resolved).then(|| Symbol::intern(&resolved)),
            None if self.has_role(&spelled) => Some(name),
            // A module-level `my role R` called from one of the module's own
            // subs is not bound in the sub's `env`; only its mangled storage
            // key (`R\u{0}<decl-id>`) says it exists.
            None => self
                .unique_lexical_type_key(&spelled)
                .filter(|key| self.has_role(key))
                .or_else(|| self.package_lexical_role_key(&spelled))
                .map(|key| Symbol::intern(&key)),
        }
    }
}

/// Whether `sym` is the ADR-0047 storage name of a lexical type spelled
/// `name` (`Name\u{0}<decl-id>`, possibly package-qualified).
// Cost: O(|sym|).
fn is_lexical_storage_of(sym: Symbol, name: &str) -> bool {
    crate::qualified::unqualified_part(sym).with_str(|short| {
        short
            .split_once('\u{0}')
            .is_some_and(|(head, _)| head == name)
    })
}

/// Whether `code` declares no type of its own: no class, role, enum, subset
/// or package declaration among its ops.
///
/// Then a lexical type its barewords resolve to was declared in an enclosing
/// scope, and that scope's block cannot begin or end inside this chunk (it
/// would contain the declaration). So every site of the chunk spelling the
/// name sits on the same side of the declaring block, inside it, and the
/// per-chunk memo answers each of them alike, as rakudo's compile-time
/// resolution of the name does. A role or class body's `my class` read from
/// its methods is this shape (AttrX::Mooish's `AttrProxy`, `NO-VALUE-YET`).
// Cost: O(1) after the first call per chunk (memoized); the first is O(o),
// o = the chunk's ops.
fn chunk_declares_no_types(code: &CompiledCode) -> bool {
    code.bareword_sites.declares_no_types(|| {
        code.class_decl_plans.is_empty()
            && code.role_decl_plans.is_empty()
            && !code.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::RegisterEnum(..)
                        | OpCode::RegisterSubset { .. }
                        | OpCode::RegisterPackage { .. }
                        | OpCode::RegisterPackageMy { .. }
                        | OpCode::RegisterPackageStub { .. }
                )
            })
    })
}

/// Whether a bareword spelled `name` may be remembered once it resolved to
/// the type object of that same spelling: not when the chain could have
/// rewritten it from a binding stored under another key (see the module
/// comment).
// Cost: O(|name|).
fn bareword_memoizable(name: &str) -> bool {
    !name.contains('[') && crate::runtime::types::strip_type_smiley(name).1.is_none()
}

/// Whether `a` and `b` are the same memoizable bareword answer: one type
/// object, or one enum value (by type and key).
// Cost: O(1).
fn same_memo_value(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::Package(x), ValueView::Package(y)) => x == y,
        (
            ValueView::Enum {
                enum_type: t1,
                key: k1,
                ..
            },
            ValueView::Enum {
                enum_type: t2,
                key: k2,
                ..
            },
        ) => t1 == t2 && k1 == k2,
        _ => false,
    }
}
