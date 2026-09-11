//! The bare-name namespace for enum keys.
//!
//! In Raku an enum's keys live in the *package symbol* namespace: `enum U «:s<time>»`
//! declares a term `s`, not a `$`-sigiled scalar. The two are distinct symbols, so
//! `my $s` and the enum key `s` coexist and neither can see or clobber the other.
//!
//! mutsu's `env`, however, stores a scalar `$s` under the sigil-less key `"s"`, so
//! installing an enum key under its own bare name put both symbols in ONE namespace
//! ([#7914](https://github.com/tokuhirom/mutsu/issues/7914)). That collision went
//! both ways:
//!
//! - a loaded module's `our Str enum U «… :s<time> …»` made the caller's `my $s`
//!   read as `Any` (importing the key recorded `s` as an env write owing a
//!   caller-slot writeback, so the next frame reconcile pulled `env["s"]` — by then
//!   that lexical's own decl-seed placeholder — over the slot holding its value), and
//! - a bare `s` term read back whatever the same-named *scalar* last held, instead
//!   of the enum value.
//!
//! The fix is a namespace, not a per-site patch: an enum key is stored under a
//! reserved key prefix that no user symbol can spell, and ONLY term/bareword
//! resolution ([`Interpreter::enum_bare_value`]) consults it. Variable lookup —
//! which reaches `env` under the sigil-less name — can no longer see it, and an
//! assignment to `$s` can no longer overwrite it.
//!
//! The storage is still `env` on purpose. Enum-key visibility is *lexical*, and
//! `env` is what implements that: block scopes, package-block rollback (a bare key
//! introduced inside `package Foo { … }` is dropped on exit, only `::`-qualified
//! keys survive — the prefix deliberately contains no `::` so an enum key keeps
//! exactly that behaviour), thread clones and the `our` store all key off it. Moving
//! the values to a side table would have had to reimplement every one of those.

use crate::runtime::Interpreter;
use crate::value::Value;

/// Key prefix under which an enum key's value is stored in `env`.
///
/// `__mutsu_`-prefixed keys are already reserved for interpreter-internal env
/// entries (they are excluded from `package_lexicals` snapshots and from the
/// "plain user variable" predicates), so an enum key parked here cannot be reached
/// by any spelling of a user variable.
pub(crate) const ENUM_BARE_PREFIX: &str = "__mutsu_enum_bare_";

/// The `env` key an enum key `name` is stored under.
pub(crate) fn enum_bare_key(name: &str) -> String {
    format!("{ENUM_BARE_PREFIX}{name}")
}

/// Latched once any enum key has been installed, so the probe on the bareword hot
/// path costs an atomic load instead of a `format!` for every program that declares
/// no enum at all. Monotonic and set strictly before any read that could observe the
/// key, exactly like `env`'s own `*_KEY_SEEN` gates; an over-set only makes the
/// (correct) probe run.
static ENUM_BARE_KEY_SEEN: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// [`enum_bare_key`] for a caller that is about to INSERT under the returned key
/// without going through [`Interpreter::insert_enum_bare_value`] — the import path
/// shares one `env.insert` between the enum-key and the ordinary case. Latches the
/// probe gate, which that insert would otherwise leave unset.
pub(crate) fn enum_bare_key_for_insert(name: &str) -> String {
    ENUM_BARE_KEY_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
    enum_bare_key(name)
}

impl Interpreter {
    /// Install an enum key's value in the bare-name namespace.
    pub(crate) fn insert_enum_bare_value(&mut self, name: &str, value: Value) {
        ENUM_BARE_KEY_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
        self.env.insert(enum_bare_key(name), value);
    }

    /// Look an enum key up in the bare-name namespace.
    ///
    /// This is the ONLY way the value is reachable by its bare spelling — a plain
    /// `env` probe under `name` deliberately misses it.
    pub(crate) fn enum_bare_value(&self, name: &str) -> Option<&Value> {
        // Two misses that are free to rule out before paying for the key:
        // the program has declared no enum at all (`ENUM_BARE_KEY_SEEN`), and a
        // QUALIFIED name (`U::s`), which the registration loop stores under its own
        // `env` key and which is not part of this namespace. Both would otherwise
        // cost a `format!` on every bareword resolution.
        if !ENUM_BARE_KEY_SEEN.load(std::sync::atomic::Ordering::Relaxed)
            || name.is_empty()
            || crate::runtime::utils::has_double_colon(name)
        {
            return None;
        }
        self.env.get(&enum_bare_key(name))
    }

    /// Reject a bare enum-key read whose name was declared by more than one enum.
    ///
    /// Raku "poisons" such an alias: only the package-qualified spelling
    /// (`Pkg::name`) may be used. Shared by the enum-key namespace probe and the
    /// legacy `env`-hit branch of bareword resolution, which must report the same
    /// `X::PoisonedAlias`.
    pub(crate) fn poisoned_enum_alias_check(
        &self,
        name: &str,
    ) -> Result<(), crate::value::RuntimeError> {
        if crate::runtime::utils::has_double_colon(name) {
            return Ok(());
        }
        let Some(pkg_name) = self.is_poisoned_enum_alias(name) else {
            return Ok(());
        };
        let pkg_name = pkg_name.to_string();
        let message = format!(
            "Cannot directly use poisoned alias '{name}' because it was declared by \
             several enums. Please access it via explicit package name like: \
             '{pkg_name}::{name}'"
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        attrs.insert("alias".to_string(), Value::str(name.to_string()));
        attrs.insert("package-name".to_string(), Value::str(pkg_name));
        attrs.insert("package-type".to_string(), Value::str("enum".to_string()));
        let ex = Value::make_instance(crate::symbol::Symbol::intern("X::PoisonedAlias"), attrs);
        let mut err = crate::value::RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        Err(err)
    }
}
