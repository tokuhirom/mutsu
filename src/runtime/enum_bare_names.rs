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
//! The storage is `env` for every enum the running program declares itself.
//! Enum-key visibility is *lexical*, and `env` is what implements that: block
//! scopes, package-block rollback (a bare key introduced inside `package Foo { … }`
//! is dropped on exit, only `::`-qualified keys survive — the prefix deliberately
//! contains no `::` so an enum key keeps exactly that behaviour), thread clones and
//! the `our` store all key off it.
//!
//! ## A module's top-level enums (ADR-0084, #7817)
//!
//! The exception is an enum a loaded module's mainline declares directly (the
//! depth rule of ADR-0084 §7.2, [`Interpreter::at_module_toplevel`]). A module
//! body runs in the IMPORTER's env, so its keys stayed behind in every frame env
//! of the program that loaded it — 44 of them after
//! `use Cro::HTTP2::RequestParser`, the largest group of non-lexical entries each
//! copy-on-write deep copy of a frame env still had to copy. They go to
//! [`ModuleToplevel::enum_keys`](super::toplevel_callable_ids::ModuleToplevel::enum_keys)
//! instead, keyed by the declaring package, which frames neither clone nor
//! capture. That also gives them rakudo's visibility, which the env could not:
//!
//! - a **package-less** module file declares its enum in `GLOBAL`, so its keys
//!   are visible everywhere, the importer included (`enum Settings <…>` in
//!   `Cro::HTTP2::Frame`);
//! - an enum declared inside a **package** — a `unit module`, a `module M { … }`
//!   block or a class body, including a `my enum` there — is visible to that
//!   package's own code (found through the running package's chain,
//!   [`Interpreter::lookup_in_running_package`]) and not to the importer. In the
//!   env, a class body's `my enum` keys used to leak into the loading scope
//!   (the class-body exit leaves them, for the sake of a `my enum` inside a
//!   method).
//!
//! [`Interpreter::enum_bare_value`] consults the env first, so a lexical key —
//! an import, the program's own enum — shadows a table entry. A closure created
//! in a package's code needs nothing captured: it runs with its declaring
//! package as its lexical package, so the running package's chain still finds
//! the key when the closure is called (or a `supply` block tapped) from outside.

use crate::runtime::Interpreter;
use crate::value::Value;

/// Key prefix under which an enum key's value is stored in `env`.
///
/// `__mutsu_`-prefixed keys are already reserved for interpreter-internal env
/// entries (they are excluded from `package_lexicals` snapshots and from the
/// "plain user variable" predicates), so an enum key parked here cannot be reached
/// by any spelling of a user variable.
pub(crate) const ENUM_BARE_PREFIX: &str = crate::meta_ns::ENUM_BARE_PREFIX;

/// The owner key of a package-less module's top-level enum keys.
const GLOBAL_OWNER: &str = "GLOBAL";

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

/// True once any enum key has been installed in this process.
pub(crate) fn enum_bare_keys_in_use() -> bool {
    ENUM_BARE_KEY_SEEN.load(std::sync::atomic::Ordering::Relaxed)
}

impl Interpreter {
    /// Install an enum key's value in the bare-name namespace.
    // Cost: O(|name|) to build the key, plus one amortized O(1) insert (a
    // copy-on-write table clone, O(t), only while a spawned thread still
    // shares the table, t = recorded keys).
    pub(crate) fn insert_enum_bare_value(&mut self, name: &str, value: Value) {
        ENUM_BARE_KEY_SEEN.store(true, std::sync::atomic::Ordering::Relaxed);
        let key = enum_bare_key(name);
        // An env binding of the same key (an earlier lexical declaration still
        // in scope) would shadow the table; overwrite it in place instead.
        if self.at_module_toplevel() && !self.env.contains_key(&key) {
            let owner = if self.current_package_is_global() {
                GLOBAL_OWNER
            } else {
                self.current_package_str()
            };
            crate::runtime::cow_table_mut(&mut self.module.module_toplevel.enum_keys)
                .entry(owner.to_string())
                .or_default()
                .insert(name.to_string(), value);
            return;
        }
        self.env.insert(key, value);
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
            || crate::qualified::is_qualified_str(name)
        {
            return None;
        }
        self.env
            .get(&enum_bare_key(name))
            .or_else(|| self.toplevel_enum_key(name))
    }

    /// An enum key a loaded module's mainline declared at its top level and
    /// that the running code can see: one of the running package's chain, else
    /// a package-less module's (`GLOBAL`) key.
    // Cost: O(1) when no package holds `name`; else O(c * d), c = running
    // package candidates (at most 4), d = package nesting depth.
    fn toplevel_enum_key(&self, name: &str) -> Option<&Value> {
        let table = &self.module.module_toplevel.enum_keys;
        if table.is_empty() || !table.contains_name(name) {
            return None;
        }
        self.lookup_in_running_package(table, name)
            .or_else(|| table.get(GLOBAL_OWNER).and_then(|keys| keys.get(name)))
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
        if crate::qualified::is_qualified_str(name) {
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
