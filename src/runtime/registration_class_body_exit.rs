//! Named phases of `register_class_decl` (ADR-0019 D0): leaving the class
//! body — LEAVE phasers, class-body static persistence, nested-type
//! short-name restoration — plus the final registration (MRO, validation,
//! rollback on failure) and custom-HOW installation. Pure mechanical
//! extraction from `registration_class_decl.rs` — no behavior change.

use super::registration_class_body::{ClassBodyCx, ClassBodyLeavePhaser};
use super::registration_class_validate::ClassRegSnapshot;
use super::*;

impl Interpreter {
    /// Fire class-body LEAVE phasers in LIFO order now that the body scope
    /// is being left. They run while the class package/env are still active
    /// so their bodies can see body-scoped variables; writes to outer
    /// variables persist because the class-body env is not rolled back on
    /// the success path.
    pub(super) fn run_class_body_leave_phasers(
        &mut self,
        cx: &ClassBodyCx<'_>,
        class_leave_phasers: &[ClassBodyLeavePhaser],
    ) -> Result<(), RuntimeError> {
        for phaser in class_leave_phasers.iter().rev() {
            self.run_class_body_chunk_or_raw(phaser.chunk.as_ref(), &phaser.body)?;
            for outer_name in cx.saved_env.keys() {
                let class_scoped_name = format!("{}::{}", cx.name, outer_name);
                if let Some(updated) = self.env.get(&class_scoped_name).cloned() {
                    self.env.insert_sym(*outer_name, updated);
                }
            }
        }
        Ok(())
    }

    /// Persist the class body's own `my` lexicals (`class C { my $x = ...;
    /// method m { $x } }`) into `package_lexicals[C]` so a method that reads
    /// them still resolves after the class-body env is gone. On the success
    /// path the body env is normally left intact, so a top-level `class`/`use`
    /// keeps the values in the mainline env and this store is merely a mirror.
    /// But when the class is loaded via `require` *inside a sub*, the loading
    /// frame's env (holding the body statics) is discarded on return, and a
    /// later method read would otherwise miss the value entirely (e.g. an
    /// initialized `my Lock $lock = Lock.new` reads back as `Any`). Mirrors
    /// the package-block store in `exec_package_scope_op`. `current_package`
    /// is still `name` here, which is exactly when a method of this class
    /// reads these names, so the store is correctly scoped.
    pub(super) fn persist_class_body_statics(
        &mut self,
        cx: &ClassBodyCx<'_>,
        declared_static_names: &[crate::symbol::Symbol],
    ) {
        // Names this class body actually `my`/`state`-declared at top level
        // (ADR-0019 D6-1: precomputed by the compiler at plan lowering
        // instead of re-walked here). A class-body static is normally
        // recognized by being NEW in `env` (absent from `saved_env`). But a
        // same-named lexical leaked into the persistent mainline env by an
        // EARLIER module's class body (the success path leaves body statics
        // in `env`) makes `saved_env` already carry the name, so this
        // class's own `my $x` would be wrongly skipped and never registered
        // as a static — its methods then fall back to the leaked bare-name
        // global (e.g. `HTTP::Message`/`HTTP::Request` both `my $CRLF`). A
        // name explicitly declared here IS a static regardless of any
        // pre-existing outer/leaked binding, so recognize it directly from
        // the precomputed declaration list.
        let name = cx.name;
        let saved_env = &cx.saved_env;
        let declared_statics: std::collections::HashSet<String> = declared_static_names
            .iter()
            .map(|sym| sym.resolve())
            .collect();
        let mut body_lexicals: Vec<(String, Value)> = self
            .env
            .iter()
            .filter_map(|(k, v)| {
                let bare = k.resolve();
                if saved_env.contains_key_sym(*k) && !declared_statics.contains(bare.as_str()) {
                    return None;
                }
                if bare.contains("::")
                    || bare.starts_with("__")
                    || bare.starts_with('?')
                    || bare.starts_with('!')
                    || bare == "self"
                    || bare == "_"
                {
                    return None;
                }
                let qualified = format!("{name}::{bare}");
                if declared_statics.contains(bare.as_str()) {
                    // A declared `my` static reassigned by a LATER body
                    // statement compiles the write package-qualified
                    // (`emit_set_named_var`), and the SetGlobal handler mirrors
                    // every package-qualified write into `our_vars` too (not
                    // only genuine `our` declarations — see the `set_our_var`
                    // call in vm_exec_dispatch.rs's SetGlobal arm), so check
                    // both stores for the qualified key and prefer it over the
                    // stale bare declaration-time snapshot.
                    if let Some(qv) = self
                        .get_our_var(&qualified)
                        .cloned()
                        .or_else(|| self.env.get(&qualified).cloned())
                    {
                        return Some((bare, qv));
                    }
                } else {
                    // Skip `our` package vars: their authoritative value lives
                    // in the qualified `our` store and can be set from outside
                    // the package; a bare declaration-time snapshot here would
                    // stale-shadow it.
                    if self.get_our_var(&qualified).is_some() || self.env.contains_key(&qualified) {
                        return None;
                    }
                }
                // Skip the short-name binding a *nested* package declaration left
                // in this body's env. `class O { class I { class C {} } }` binds a
                // bare `C` (so `C` resolves inside `I`'s body), and because a class
                // body deliberately does not restore `env` on success, that binding
                // is still present when `O`'s body finishes — making `O` look like
                // it declared a class-body `my C`. That in turn makes every method
                // of `O` switch `current_package` to `O`, so a method-body `sub`
                // registers under `O::` while a lazily-forced `gather` body resolves
                // it under `GLOBAL::` and cannot find it. A package type object is
                // never a class-body static unless the body really did `my $C = ...`,
                // which `declared_statics` records.
                if !declared_statics.contains(bare.as_str())
                    && matches!(v.view(), ValueView::Package(_))
                {
                    return None;
                }
                Some((bare, v.clone()))
            })
            .collect();
        // A sigiled class-body static is stored under the sigil-leading
        // package-qualified spelling (`&State::tzparse`, `@State::items`, ...),
        // while the declaration's local name is `&tzparse`/`@items`. The
        // ordinary body walk also leaves the declaration-time type object under
        // the local key, so prefer the qualified store's final value here.
        // Without this, a later `&name = sub {...}` assignment was persisted as
        // the `Callable` type object and bare calls from the class's own subs
        // resolved to an unusable placeholder.
        for static_name in declared_static_names {
            let static_name = static_name.resolve();
            let (sigil, bare_name) = match static_name.as_bytes().first() {
                Some(b'$' | b'@' | b'%' | b'&') => static_name.split_at(1),
                _ => ("", static_name.as_str()),
            };
            let qualified_symbol = crate::qualified::qualified(
                crate::symbol::Symbol::intern(name),
                crate::symbol::Symbol::intern(bare_name),
            );
            let qualified = if sigil.is_empty() {
                qualified_symbol.as_str().to_string()
            } else {
                format!("{sigil}{}", qualified_symbol.as_str())
            };
            let Some(value) = self
                .get_our_var(&qualified)
                .cloned()
                .or_else(|| self.env.get(&qualified).cloned())
            else {
                continue;
            };
            if let Some((_, existing)) = body_lexicals
                .iter_mut()
                .find(|(bare, _)| bare == &static_name)
            {
                *existing = value;
            } else {
                body_lexicals.push((static_name.to_string(), value));
            }
        }
        if !body_lexicals.is_empty() {
            let marks = crate::runtime::cow_table_mut(&mut self.class_body_static_names)
                .entry(name.to_string())
                .or_default();
            for (bare, _) in &body_lexicals {
                marks.insert(bare.clone());
            }
            let store = self
                .package_lexicals_cow_mut()
                .entry(name.to_string())
                .or_default();
            // Only the names this body genuinely `my`-declared are unbound below.
            // `body_lexicals` deliberately over-approximates — for a `unit class`
            // it also picks up everything the body's own `use` statements imported
            // (`unit class HTTP::UserAgent; use HTTP::UserAgent::Common;` brings in
            // that module's `%useragents`). Recording those as statics is harmless;
            // *unbinding* them is not, and it broke the exported `get-ua` that
            // reads `%useragents`.
            let mut static_names = Vec::with_capacity(body_lexicals.len());
            for (bare, v) in body_lexicals {
                if declared_statics.contains(bare.as_str()) {
                    static_names.push(bare.clone());
                }
                store.insert(bare, v);
            }
            // The class body is these names' whole scope. Leaving the bare
            // binding in the env made every class-body `my` a de facto global:
            // the next class body declaring the same name overwrote it, and the
            // FIRST class's methods then read the SECOND class's value. Cro has
            // four `my constant @defaults`, one per selector class, so every
            // body-parser lookup ran the body *serializer* list. Restore each name
            // to whatever the enclosing scope had — the authoritative copy now
            // lives in `package_lexicals`, which method dispatch injects.
            for bare in static_names {
                match saved_env.get(&bare) {
                    Some(previous) => {
                        let previous = previous.clone();
                        self.env.insert(bare, previous);
                    }
                    None => {
                        self.env.remove(&bare);
                    }
                }
            }
        }
    }

    /// A type declared inside this class body (`class Outer { my class Inner
    /// {} }`) binds its SHORT name in the env so the rest of the body can name
    /// it. That binding must not outlive the body: the class body is the name's
    /// scope, and leaving it behind clobbers a same-named file-scope class for
    /// the rest of the program, and out-lives any later inner-scope declaration
    /// of the same name (a `my enum <... Header ...>` in a supply block could
    /// not shadow `Cro::HTTP::Header`'s `my grammar Header`). Methods do not
    /// need the binding — `resolve_suppressed_type` resolves a nested short
    /// name through the owner package chain — so restore each one to whatever
    /// the enclosing scope had.
    pub(super) fn restore_nested_type_short_names(&mut self, cx: &ClassBodyCx<'_>) {
        let name = cx.name;
        let saved_env = &cx.saved_env;
        let nested_short_names: Vec<String> = self
            .env
            .iter()
            .filter_map(|(k, v)| {
                let bare = k.resolve();
                if bare.contains("::") {
                    return None;
                }
                let ValueView::Package(p) = v.view() else {
                    return None;
                };
                let target = p.resolve();
                let prefix = format!("{name}::{bare}");
                // The storage name is either exactly `Outer::Inner` or the
                // mangled `Outer::Inner\0<decl-id>` form a lexical class gets
                // when it collides with an out-of-scope namesake.
                (target == prefix || target.starts_with(&format!("{prefix}\u{0}")))
                    .then(|| bare.to_string())
            })
            .collect();
        for bare in nested_short_names {
            match saved_env.get(&bare) {
                Some(previous) => {
                    let previous = previous.clone();
                    self.env.insert(bare, previous);
                }
                None => {
                    self.env.remove(&bare);
                }
            }
        }
    }

    /// Final registration of the composed class: resolve stub requirements,
    /// detect role-method conflicts, publish the class, compute its MRO, and
    /// validate private-method calls — rolling back to the snapshot on any
    /// failure. Also arms the metamodel-HOW dispatch gate.
    pub(super) fn finalize_class_registration(
        &mut self,
        name: &str,
        parents: &[String],
        class_def: ClassDef,
        snapshot: &ClassRegSnapshot,
        is_hoisted_shell: bool,
    ) -> Result<(), RuntimeError> {
        if let Err(err) = self.resolve_class_stub_requirements(name) {
            snapshot.restore(self, name);
            return Err(err);
        }
        if let Err(err) = self.detect_unresolved_role_method_conflicts(name) {
            snapshot.restore(self, name);
            return Err(err);
        }
        self.registry_mut()
            .classes
            .insert(name.to_string(), class_def);
        // The per-op walk in `run_class_body` re-derives the accessor column
        // after each body statement so mid-body introspection sees it, but a
        // class whose attributes arrive ONLY from header-level composition
        // (`class Bar is Foo does R {}`, an empty body) never runs that walk
        // at all, so `registry.classes` was published in `publish_class_shell`
        // and re-published just above with `method_entries[].accessor` still
        // reflecting whatever the previous declaration (or nothing) left
        // behind. A role-composed public attribute then lost to a same-named
        // *inherited* method at this class's own MRO level (`resolve_user_
        // method_or_accessor` never saw `has_attr` here and fell through to
        // the ancestor's method) — the accessor is meant to win at this
        // level per `resolve_user_method_or_accessor`'s doc comment. This is
        // the FINAL class_def for this declaration, so re-derive
        // unconditionally rather than depend on the body having had a chance
        // to.
        self.registry_mut()
            .sync_accessor_entries(crate::symbol::Symbol::intern(name));
        let mut stack = Vec::new();
        match self.compute_class_mro(name, &mut stack) {
            Err(err) => {
                snapshot.restore(self, name);
                return Err(err);
            }
            // Memoize the linearization this validation just computed, so a
            // subclass declared next reads it instead of re-walking the whole
            // ancestor chain -- without it a chain of d classes cost O(d^3) to
            // declare (#9173). A `__hoisted` shell is skipped: the real
            // declaration re-registers it and computes its own.
            Ok(mro) if !is_hoisted_shell => {
                let mro: std::sync::Arc<[crate::symbol::Symbol]> = mro
                    .iter()
                    .map(|s| crate::symbol::Symbol::intern(s))
                    .collect();
                if let Some(class_def) = self.registry_mut().classes.get_mut(name)
                    && class_def.mro.is_empty()
                {
                    class_def.mro = mro;
                }
            }
            Ok(_) => {}
        }
        // Validate that all self!method() calls reference existing private
        // methods. Skip this for a `__hoisted` forward-reference shell
        // (`hoist_type_decl_shells`): its registration runs before the
        // class's own body statements (including a `BEGIN { ... }` that
        // dynamically composes a role via `.^add_role`) have executed, so a
        // private method the role would supply is not visible yet even
        // though the real, source-position declaration below will compose
        // it correctly. The shell's registration is discarded and
        // re-validated by that real declaration anyway (see
        // `ClassDeclModifiers::is_hoisted_shell`'s doc comment), so a
        // validation failure here is a false positive, not a genuine error
        // (#8573).
        if !is_hoisted_shell && let Err(err) = self.validate_private_method_existence(name) {
            snapshot.restore(self, name);
            return Err(err);
        }
        // A user class inheriting a builtin metamodel class is a HOW subclass:
        // arm the cheap per-dispatch gate for `is_metamodel_how_class`. Check
        // the raw parent list too — builtin parents that are not registry
        // classes may be absent from the computed MRO.
        if !self.registry().has_metamodel_how_classes
            && (parents.iter().any(|c| Self::is_metamodel_class_name(c))
                || self.registry().classes.get(name).is_some_and(|cd| {
                    cd.mro
                        .iter()
                        .any(|c| Self::is_metamodel_class_name(c.as_str()))
                }))
        {
            self.registry_mut().has_metamodel_how_classes = true;
        }
        Ok(())
    }

    /// Install a custom EXPORTHOW metaclass on the freshly-declared class,
    /// when one is mapped for its declarator.
    pub(super) fn install_class_exporthow(
        &mut self,
        name: &str,
        parents: &[String],
    ) -> Result<(), RuntimeError> {
        // A grammar declared while an EXPORTHOW `grammar` metaclass mapping is
        // installed (`EXPORTHOW.WHO.<grammar> = SomeHOW`, typically from a
        // `use`d module) gets an instance of that HOW. The regex engine then
        // routes the grammar's subrule dispatch through the HOW's user
        // `find_method` (Metamodel::GrammarHOW protocol).
        // TODO: EXPORTHOW should be lexically scoped to the `use`ing scope;
        // mutsu approximates it with the (globally visible) EXPORTHOW package
        // stash entry.
        // The stash-assignment path stores the entry under a `$`-prefixed env
        // key (`EXPORTHOW::$grammar`); check the sigil-less form too.
        if parents.iter().any(|p| p == "Grammar")
            && let Some(how_type) = self
                .env
                .get("EXPORTHOW::$grammar")
                .or_else(|| self.env.get("EXPORTHOW::grammar"))
                .cloned()
        {
            self.install_custom_grammar_how(name, how_type)?;
        }
        // A non-grammar `class` declared while an EXPORTHOW `class` metaclass
        // mapping is installed (`EXPORTHOW.WHO.<class> = SomeHOW`, from a `use`d
        // module) gets an instance of that HOW as its meta-object, so
        // `TheClass.HOW.<method>` dispatches to the custom HOW (e.g. the AOP
        // example's `add_aspect`, called by a user `trait_mod:<is>`). Installed
        // BEFORE the caller applies this class's custom `is` traits so those
        // reach the custom HOW; if the HOW defines a user `compose`, the class is
        // queued for a post-trait `compose` call (`advent2011-day14`).
        else if let Some(how_type) = self
            .env
            .get("EXPORTHOW::$class")
            .or_else(|| self.env.get("EXPORTHOW::class"))
            .cloned()
            && self.install_custom_class_how(name, how_type)?
        {
            self.registry_mut()
                .pending_class_compose
                .push(name.to_string());
        }
        Ok(())
    }
}
