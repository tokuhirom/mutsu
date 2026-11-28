//! The per-site memo for a typed declaration of a user class or role
//! (`my Tag $t = ...`), and the Nil seeds of typed scalars.
//!
//! `SetVarTypeScoped`, the declaration's `TypeCheck` and a Nil store each
//! resolved the constraint spelling on every execution: the type-capture,
//! package-alias, constant-alias and lexical-type probes, the package-chain
//! qualification, and `nominal_type_object_name_for_constraint` for the type
//! object a Nil leaves in the variable. For a spelling that names a class or
//! role registered under exactly that name, every one of those answers is a
//! function of the registry, the running package and a few env bindings. The
//! registry and package parts are memoized per site
//! ([`crate::value::TypeDeclSiteCaches`]); the env parts are asked again on
//! every hit, so a binding that appears later still takes effect (#12917).
use super::*;

impl Interpreter {
    /// Whether the declaration constraint at constant `tc_idx` names a user
    /// class or role whose every resolution step returns the spelling
    /// unchanged — so the declaration may register it as written and seed a
    /// Nil with the type object of that very name.
    ///
    /// Asked on every call, because they read state the registry generation
    /// does not cover: a `::T` capture under the spelling (or under its head,
    /// which the capture resolver splits off at the first `:`), a module's
    /// package alias of it, a `CORE::` pin in progress, and the two env
    /// bindings [`Self::decl_type_name_rebound_in_env`] checks. Memoized per
    /// site for one (registry write generation, running package): the
    /// registry membership, the alias-chain and package-chain resolution, and
    /// the nominal type-object name.
    // Cost: O(|name|) plus one or two env probes on a memo hit; a miss adds
    // the full resolution, O(p), p = packages probed.
    pub(super) fn type_decl_constraint_is_plain_user_type(
        &self,
        code: &CompiledCode,
        tc_idx: u32,
        constraint: &str,
    ) -> bool {
        if self.types.core_pin_depth > 0 || !Self::plain_user_type_spelling(constraint) {
            return false;
        }
        let head = constraint.split(':').next().unwrap_or(constraint);
        if self.has_type_capture_binding(constraint)
            || (head.len() != constraint.len() && self.has_type_capture_binding(head))
            || self.types.package_type_aliases.contains_name(constraint)
        {
            return false;
        }
        let generation = self.registry_write_generation();
        let Some(key) =
            crate::value::TypeDeclSiteCaches::user_key(generation, self.current_package_sym().id())
        else {
            return false;
        };
        let sites = code.constants.len();
        let idx = tc_idx as usize;
        if !code.type_decl_sites.user_cached(sites, idx, key) {
            if !self.user_type_constraint_resolves_to_itself(constraint) {
                return false;
            }
            if self.registry_write_generation() == generation {
                code.type_decl_sites.remember_user(sites, idx, key);
            }
        }
        !self.decl_type_name_rebound_in_env(code.const_sym(tc_idx))
    }

    /// A type name and nothing else: optionally package-qualified (`A::B`),
    /// with no smiley, coercion, parameterization, hash-key part, capture
    /// sigil, `CORE::` pin or lexical storage mangling.
    // Cost: O(|name|).
    fn plain_user_type_spelling(constraint: &str) -> bool {
        let bytes = constraint.as_bytes();
        if bytes.is_empty() || constraint.starts_with("CORE::") {
            return false;
        }
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b':' => {
                    // Only a `::` separator with a name on both sides.
                    if i == 0 || bytes.get(i + 1) != Some(&b':') || i + 2 >= bytes.len() {
                        return false;
                    }
                    if bytes[i + 2] == b':' {
                        return false;
                    }
                    i += 2;
                }
                b'(' | b')' | b'[' | b']' | b'{' | b'}' | b' ' | b'<' | b'\0' => return false,
                _ => i += 1,
            }
        }
        true
    }

    /// The memoized half of [`Self::type_decl_constraint_is_plain_user_type`]:
    /// `constraint` is a class or role registered under that very name (not
    /// a builtin, subset or enum spelling), and the resolution
    /// `exec_set_var_type` and the Nil seed run return it unchanged.
    // Cost: O(p), p = packages probed by the qualification walk.
    fn user_type_constraint_resolves_to_itself(&self, constraint: &str) -> bool {
        if crate::runtime::utils::is_known_type_constraint(constraint)
            || Self::is_builtin_type(constraint)
        {
            return false;
        }
        {
            let reg = self.registry();
            if !(reg.classes.contains_key(constraint) || reg.roles.contains_key(constraint))
                || reg.subsets.contains_key(constraint)
                || reg.enum_types.contains_key(constraint)
            {
                return false;
            }
        }
        if self.try_resolved_type_capture_name(constraint).is_some() {
            return false;
        }
        if !crate::qualified::is_qualified_str(constraint)
            && self
                .resolve_type_in_current_package(constraint)
                .is_some_and(|resolved| resolved != constraint)
        {
            return false;
        }
        self.nominal_type_object_name_for_constraint(constraint) == constraint
    }

    /// Whether an env binding now makes the spelling `sym` resolve to
    /// something other than itself: a lexical type in scope under it (what
    /// `try_resolved_type_capture_name` substitutes), or a `Package` bound
    /// under it naming another type (what the Nil seed's
    /// `nominal_type_object_name_for_constraint` follows).
    // Cost: one env probe, two for an unqualified name.
    pub(super) fn decl_type_name_rebound_in_env(&self, sym: Symbol) -> bool {
        if !crate::qualified::is_qualified(sym)
            && self.bound_lexical_type_storage(sym.as_str()).is_some()
        {
            return true;
        }
        crate::symbol::maybe_env_key(sym)
            && matches!(
                self.env().get_sym(sym).map(Value::view),
                Some(ValueView::Package(bound)) if bound != sym
            )
    }

    /// Whether a declaration whose constraint resolved as written seeds a
    /// Nil with the type object of the constant's own name: a plain user type
    /// (whose answer already asked the env), or a plain, non-native builtin
    /// (the constant's symbol) registered by a routine-scoped op -- whose seed the general path reads
    /// back from that very registration -- that no env binding renames.
    // Cost: O(1) for a user type; one env probe for a builtin.
    pub(super) fn decl_site_seeds_as_written(
        &self,
        plain_user: bool,
        plain_builtin: bool,
        scoped: bool,
        constraint: Symbol,
    ) -> bool {
        plain_user
            || plain_builtin
                && scoped
                && Self::native_nil_seed_value(constraint.as_str()).is_none()
                && !self.decl_type_name_rebound_in_env(constraint)
    }

    /// The value a `SetVarType*` op seeds a Nil-valued typed scalar with:
    /// the type object of the constant's own name when the site resolved as
    /// written ([`Self::decl_site_seeds_as_written`]; `site` is its
    /// `(plain_user, plain_builtin, scoped)`), else the native zero or the
    /// general nominal seed of the registered constraint.
    // Cost: O(1) on the site lane; else as `typed_scalar_nil_seed_value_with_base`.
    pub(super) fn decl_nil_seed(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        name_sym: Symbol,
        constraint: &str,
        site: (bool, bool, bool),
    ) -> Value {
        let (plain_user, plain_builtin, scoped) = site;
        let written = code.const_sym(tc_idx);
        if self.decl_site_seeds_as_written(plain_user, plain_builtin, scoped, written) {
            return Value::package(written);
        }
        match self.unshadowed_native_nil_seed(constraint) {
            Some(native) => native,
            None => {
                let base = self.var_type_constraint_sym(name_sym);
                self.typed_scalar_nil_seed_value_with_base(constraint, base.as_deref())
            }
        }
    }

    /// The value a Nil-valued typed scalar holds under constraint `constraint`:
    /// the nominal type object, except native types (zero/empty defaults) and
    /// parameterized roles (the ParametricRole type object, so `.WHAT`/`.raku`
    /// keep the type arguments). Used both by the declaration-time seeding
    /// above and by the SetLocal store path when a Nil is ASSIGNED to a typed
    /// scalar — the read paths deliberately do not consult env-scoped
    /// constraints for Nil→type-object conversion (a `= Nil` parameter default
    /// must stay Nil), so the stored value itself must carry the type object.
    pub(crate) fn typed_scalar_nil_seed_value(&mut self, name: &str, constraint: &str) -> Value {
        if !self.constraint_is_user_subset(constraint)
            && let Some(native) = Self::native_nil_seed_value(constraint)
        {
            return native;
        }
        let base = loan_env!(self, var_type_constraint(name));
        self.typed_scalar_nil_seed_value_with_base(constraint, base.as_deref())
    }

    /// [`Self::native_nil_seed_value`] unless a user subset spelled like the
    /// native type shadows it (#12359).
    // Cost: O(1).
    pub(super) fn unshadowed_native_nil_seed(&self, constraint: &str) -> Option<Value> {
        if self.constraint_is_user_subset(constraint) {
            None
        } else {
            Self::native_nil_seed_value(constraint)
        }
    }

    /// The zero/empty default of a native scalar constraint (`int` -> 0,
    /// `num` -> 0e0, `str` -> ""), which needs neither the registered base
    /// name nor a registry probe — so a caller that has to FETCH that base
    /// (an env read plus a `String` copy) asks this first and skips it
    /// (#12151).
    // Cost: O(1).
    pub(crate) fn native_nil_seed_value(constraint: &str) -> Option<Value> {
        if crate::runtime::native_types::is_native_int_type(constraint) {
            Some(Value::int(0))
        } else if matches!(constraint, "num" | "num32" | "num64") {
            Some(Value::num(0.0))
        } else if constraint == "str" {
            Some(Value::str(String::new()))
        } else {
            None
        }
    }

    /// [`Self::typed_scalar_nil_seed_value`] with the declared constraint
    /// supplied by the caller rather than read back by name — for a store
    /// whose constraint came from the cell it lands in (a routine's write to
    /// its own free variable), where the name-keyed entry may belong to an
    /// unrelated same-named variable of the calling scope (#10049).
    pub(crate) fn typed_scalar_nil_seed_value_with_base(
        &mut self,
        constraint: &str,
        base: Option<&str>,
    ) -> Value {
        // A user subset spelled like a native type shadows it (#12359).
        if !self.constraint_is_user_subset(constraint)
            && let Some(native) = Self::native_nil_seed_value(constraint)
        {
            native
        } else {
            // A parameterized role constraint (`my Cup of EggNog $mug` /
            // `my Cup[EggNog] $mug`) resolves to the ParametricRole type
            // object. The stored constraint metadata normalizes to the base
            // name, so probe the raw constraint here.
            let parametric = constraint
                .contains('[')
                .then(|| loan_env!(self, type_arg_value_from_name(constraint)));
            match parametric {
                Some(v) if matches!(v.view(), ValueView::ParametricRole { .. }) => v,
                // A class with its own `^parameterize` (upstream NativeCall's
                // `Pointer[uint16]`): the type object that meta-method built.
                _ if constraint.contains('[')
                    && let Some(v) = loan_env!(self, meta_parameterized_type(constraint)) =>
                {
                    v
                }
                _ => {
                    // The seeded package must carry the NOMINAL type name —
                    // smileys stripped (`my Int:_ $a` seeds `Int`, not
                    // `Int:_`) and coercion parens unwrapped — same as the
                    // read-path Nil→type-object conversion it replaces.
                    Value::package(
                        self.nominal_type_object_sym_for_constraint(base.unwrap_or(constraint)),
                    )
                }
            }
        }
    }
}
