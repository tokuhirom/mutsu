//! Class/role introspection helpers: native-method tables, user-method and
//! accessor presence checks, class-level attribute lookup/mutation, and
//! attribute collection across the MRO and composed roles. Lifecycle/MRO lives
//! in `class`; instance-method dispatch in `class_dispatch`.

use super::user_method_probe_memo::probe_key;
use super::*;
use crate::runtime::meta_ns::MetaNs;

/// Winner of the per-MRO-level race between an explicit user method and a
/// public attribute accessor (see `resolve_user_method_or_accessor`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum UserMethodOrAccessor {
    Method,
    Accessor,
}

impl Interpreter {
    pub(crate) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        // Read guard first: this is a pure question, and `registry_mut()`'s
        // first mutable deref deep-clones the WHOLE registry whenever a thread
        // clone shares the `Arc` (`RegistryWriteGuard::deref_mut`). A `supply`
        // block registering one `whenever` is enough to make that share
        // permanent, so asking through the write side cost one full registry
        // copy per dispatch that reached here — 8.7% of the per-frame work in
        // Cro's HTTP/2 request parser (#7667).
        if let Some(answer) = self
            .registry()
            .class_has_method_readonly(class_name, method_name)
        {
            return answer;
        }
        self.registry_mut()
            .class_has_method(class_name, method_name)
    }

    pub(super) fn class_has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        // See `class_has_method` above for why the read guard comes first.
        if let Some(answer) = self
            .registry()
            .class_has_user_method_readonly(class_name, method_name)
        {
            return answer;
        }
        self.registry_mut()
            .class_has_user_method(class_name, method_name)
    }

    /// Check whether the class (or its MRO ancestors) has a `new` method
    /// variant with a non-named positional parameter whose type matches the
    /// given value.  This is used by the coercion fallback: only try `new`
    /// when there is an explicit `new(TargetType:U: ValueType $x)` multi.
    pub(super) fn class_has_new_accepting_positional(
        &mut self,
        class_name: &str,
        value: &Value,
    ) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            let methods = self.registry().user_method_overloads(cn.as_str(), "new");
            if let Some(overloads) = methods {
                for method in &overloads {
                    // Look for a positional (non-named, non-invocant) param
                    // that type-matches the value.
                    let has_matching_positional = method.param_defs.iter().any(|pd| {
                        !pd.named
                            && !pd.is_invocant
                            // An UNTYPED positional is an `Any` parameter, which
                            // accepts the value: `method new($v) { self.bless(:$v) }`
                            // is an explicit positional constructor and rakudo
                            // coerces through it. Only the default constructor
                            // (named-only params) must stay out of coercion.
                            && pd
                                .type_constraint
                                .as_deref()
                                .is_none_or(|tc| self.type_matches_value(tc, value))
                    });
                    if has_matching_positional {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Whether any class in `class_name`'s MRO declares a user-written `new`.
    /// `false` means every `new` reachable on it is the native/default
    /// constructor, whose signature is not in the method registry — so
    /// [`Self::class_has_new_accepting_positional`], which only inspects user
    /// overloads, cannot answer for it.
    pub(super) fn class_declares_user_new(&mut self, class_name: &str) -> bool {
        self.class_mro(class_name).iter().any(|cn| {
            self.registry()
                .user_method_overloads(cn.as_str(), "new")
                .is_some()
        })
    }

    /// Hardcoded native-method names for the handful of built-in classes
    /// whose getters/setters are implemented by dedicated `native_io_*`/
    /// native dispatch helpers rather than through `ClassDef::native_methods`
    /// (the `is native(&sym)` trait registry). Exact class-name match only —
    /// does not apply to subclasses. Shared with `resolve_sequence`'s
    /// `ResolvedCandidate::NativeCallBinding` detection (ADR-0019 E4b step 3,
    /// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`)
    /// so the two stay in lockstep by construction rather than by discipline.
    pub(super) fn hardcoded_native_method(class_name: &str, method_name: &str) -> bool {
        // IO::Pipe has native methods handled by native_io_pipe
        if class_name == "IO::Pipe"
            && matches!(
                method_name,
                "slurp"
                    | "slurp-rest"
                    | "Str"
                    | "gist"
                    | "encoding"
                    | "close"
                    | "split"
                    | "print"
                    | "say"
                    | "put"
                    | "flush"
                    | "write"
                    | "get"
                    | "lines"
                    | "eof"
                    | "proc"
                    | "IO"
                    | "path"
            )
        {
            return true;
        }
        // IO::Special has native methods handled by native_io_special
        if class_name == "IO::Special"
            && matches!(
                method_name,
                "Str"
                    | "gist"
                    | "what"
                    | "IO"
                    | "e"
                    | "d"
                    | "f"
                    | "l"
                    | "x"
                    | "s"
                    | "r"
                    | "w"
                    | "modified"
                    | "accessed"
                    | "changed"
                    | "mode"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "new"
                    | "Bool"
                    | "defined"
            )
        {
            return true;
        }
        // IO::Handle has native methods handled by native_io_handle
        if class_name == "IO::Handle"
            && matches!(
                method_name,
                "DESTROY"
                    | "path"
                    | "IO"
                    | "Str"
                    | "gist"
                    | "open"
                    | "nl-out"
                    | "nl-in"
                    | "chomp"
                    | "print-nl"
                    | "close"
                    | "get"
                    | "getc"
                    | "readchars"
                    | "lines"
                    | "words"
                    | "read"
                    | "write"
                    | "print"
                    | "say"
                    | "put"
                    | "flush"
                    | "lock"
                    | "unlock"
                    | "out-buffer"
                    | "seek"
                    | "tell"
                    | "eof"
                    | "encoding"
                    | "opened"
                    | "slurp"
                    | "Supply"
                    | "native-descriptor"
                    | "spurt"
                    | "t"
                    | "printf"
                    | "split"
                    | "comb"
                    | "raku"
                    | "perl"
            )
        {
            return true;
        }
        // IO::Path's comb reads file content and combs the result.
        if class_name == "IO::Path" && method_name == "comb" {
            return true;
        }
        // Thread native methods
        if class_name == "Thread"
            && matches!(
                method_name,
                "finish"
                    | "id"
                    | "Numeric"
                    | "name"
                    | "is-initial-thread"
                    | "app_lifetime"
                    | "Str"
                    | "gist"
                    | "WHAT"
            )
        {
            return true;
        }
        // Perl6::SysConfig -- the object `nqp::gethllsym("default",
        // "SysConfig")` hands back (see `Interpreter::bootstrap_hll_syms`
        // and `native_sys_config`).
        if class_name == "Perl6::SysConfig" && method_name == "rakudo-build-config" {
            return true;
        }
        // VM native methods
        if class_name == "VM"
            && matches!(
                method_name,
                "name"
                    | "auth"
                    | "version"
                    | "osname"
                    | "precomp-ext"
                    | "precomp-target"
                    | "prefix"
                    | "desc"
                    | "signature"
                    | "config"
                    | "properties"
                    | "raku"
                    | "platform-library-name"
                    | "request-garbage-collection"
                    | "gist"
                    | "Str"
            )
        {
            return true;
        }
        false
    }

    // Cost: O(1) amortized (the MRO walk is memoized per `(class, method)` for
    // one registry write generation); a miss is O(d), d = MRO depth.
    pub(crate) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
        if Self::hardcoded_native_method(class_name, method_name) {
            return true;
        }
        match (probe_key(class_name), probe_key(method_name)) {
            (Some(class), Some(method)) => self.is_native_method_memo(class, method),
            _ => self.is_native_method_uncached(class_name, method_name),
        }
    }

    /// The MRO walk behind [`Self::is_native_method`], unmemoized.
    // Cost: O(d), d = MRO depth.
    pub(super) fn is_native_method_uncached(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str())
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        self.has_user_method_sym(class_name, crate::symbol::Symbol::intern(method_name))
    }

    /// [`Self::has_user_method`] for a caller that already holds the method
    /// name interned — the compiled dispatch entries do, and on a `Match`
    /// receiver the intern alone was a measurable share of the call (#8888).
    // Cost: O(1) amortized (memoized per `(class, method)` for one registry
    // write generation, see `user_method_probe_memo.rs`); a miss is O(d),
    // d = MRO depth.
    pub(crate) fn has_user_method_sym(
        &mut self,
        class_name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> bool {
        match probe_key(class_name) {
            Some(class) => self.has_user_method_memo(class, name_sym),
            None => self.has_user_method_uncached(class_name, name_sym),
        }
    }

    /// The MRO walk behind [`Self::has_user_method_sym`], unmemoized.
    // Cost: O(d), d = MRO depth.
    pub(super) fn has_user_method_uncached(
        &mut self,
        class_name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> bool {
        let mro = self.class_mro(class_name);
        // Symbol-keyed: the MRO is already `Symbol`s, so the per-level probe
        // interns nothing (the `&str` API re-interned owner AND name at every
        // level) and clones no candidate list (see
        // `Registry::user_method_public_presence`).
        let registry = self.registry();
        for cn in mro.iter() {
            if let Some(any_public) = registry.user_method_public_presence(*cn, name_sym) {
                return any_public;
            }
        }
        false
    }

    /// [`Self::has_user_method`] widened to a bare role name. A punned role
    /// (`Role.new`) carries the role as its type name, but the role's methods
    /// live in the role registry, which the class MRO does not reach — so a
    /// classes-only lookup answers `false` for every method a punned role
    /// declares.
    pub(crate) fn has_user_method_including_role(&mut self, name: &str, method_name: &str) -> bool {
        if self.has_user_method(name, method_name) {
            return true;
        }
        self.registry()
            .roles
            .get(name)
            .and_then(|r| r.methods.get(method_name))
            .is_some_and(|defs| defs.iter().any(|d| !d.is_private))
    }

    /// Grammar dispatch also has to see public methods declared by an
    /// un-punned role composed onto the grammar. Those methods remain in the
    /// role registry rather than the class method table, but they override
    /// Grammar's native `parse`/`subparse`/`parsefile` entry points.
    ///
    /// This sits on the per-call method-dispatch path of every instance (a
    /// `Match` receiving `.Str`/`.chars`/`.from` included), so the checks run
    /// cheapest first. `class_is_grammar` goes last: for a built-in receiver
    /// such as `Match` it is not in the class table and falls back to a
    /// `::`-tail scan of every registered class, which cost ~115k
    /// instructions per call and 44% of `bench-regex-capture`.
    // Cost: O(1) amortized (memoized per `(class, method)` for one registry
    // write generation); a miss is O(d + r), d = MRO depth, r = registered
    // roles, plus the O(d^2) grammar-ancestry walk when a role on the MRO
    // declares the method.
    pub(crate) fn grammar_has_user_method(&mut self, name: &str, method_name: &str) -> bool {
        match (probe_key(name), probe_key(method_name)) {
            (Some(class), Some(method)) => self.grammar_has_user_method_memo(class, method),
            _ => self.grammar_has_user_method_sym(name, crate::symbol::Symbol::intern(method_name)),
        }
    }

    /// [`Self::grammar_has_user_method`] for a caller that already holds the
    /// method name interned (see [`Self::has_user_method_sym`]).
    pub(crate) fn grammar_has_user_method_sym(
        &mut self,
        name: &str,
        method_sym: crate::symbol::Symbol,
    ) -> bool {
        if self.has_user_method_sym(name, method_sym) {
            return true;
        }
        let method_name = method_sym.as_str();
        let role_declares_it = {
            let registry = self.registry();
            !registry.roles.is_empty()
                && registry.roles.values().any(|role_def| {
                    role_def
                        .methods
                        .get(method_name)
                        .is_some_and(|defs| defs.iter().any(|d| !d.is_private))
                })
        };
        role_declares_it
            && self.mro_readonly(name).iter().any(|owner| {
                self.registry()
                    .roles
                    .get(owner.as_str())
                    .and_then(|role_def| role_def.methods.get(method_name))
                    .is_some_and(|defs| defs.iter().any(|d| !d.is_private))
            })
            && self.class_is_grammar(name)
    }

    /// Check if a class has a public attribute accessor for the given name.
    ///
    /// The most-derived declaration of an attribute name decides its
    /// visibility (mirroring `collect_class_attributes`' override-by-name
    /// merge), so walk the MRO derived-first and stop at the first class
    /// declaring the name — a `MethodEntry` table probe per level (ADR-0019
    /// D2d) instead of a linear scan of that class's attribute vector, since
    /// this sits on the per-call method-dispatch path.
    pub(crate) fn has_public_accessor(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        // Symbol-keyed per level: the MRO entries already ARE symbols, and the
        // method name is interned once for the whole walk.
        let name_sym = crate::symbol::Symbol::intern(method_name);
        let registry = self.registry();
        for cn in mro.iter() {
            if let Some(is_public) = registry.accessor_is_public_sym(*cn, name_sym) {
                return is_public;
            }
        }
        false
    }

    /// Decide, per MRO level, whether an explicit user method or a public
    /// attribute accessor handles `method_name` for `class_name`.
    ///
    /// In Raku the auto-generated accessor for `has $.x` is an ordinary method
    /// of its declaring class, so it participates in the MRO like any other
    /// method: a child's accessor shadows a parent's explicit method of the
    /// same name (Zef::Distribution's `has $.name` vs its parent
    /// DependencySpecification's `method name`). Within a single class level
    /// the priority is: explicit class-local method > public attribute
    /// accessor > role-composed method (class entities are prioritized over
    /// role entities — 6.c S14-roles/attributes.t "Class prioritization").
    pub(crate) fn resolve_user_method_or_accessor(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<UserMethodOrAccessor> {
        self.resolve_user_method_or_accessor_sym(
            class_name,
            crate::symbol::Symbol::intern(method_name),
        )
    }

    /// [`Self::resolve_user_method_or_accessor`] for a caller that already
    /// holds the method name interned (see [`Self::has_user_method_sym`]).
    // Cost: O(1) amortized (memoized per `(class, method)` for one registry
    // write generation, see `user_method_probe_memo.rs`); a miss is O(d),
    // d = MRO depth.
    pub(crate) fn resolve_user_method_or_accessor_sym(
        &mut self,
        class_name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> Option<UserMethodOrAccessor> {
        match probe_key(class_name) {
            Some(class) => self.resolve_user_method_or_accessor_memo(class, name_sym),
            None => self.resolve_user_method_or_accessor_uncached(class_name, name_sym),
        }
    }

    /// The MRO walk behind [`Self::resolve_user_method_or_accessor_sym`],
    /// unmemoized.
    // Cost: O(d), d = MRO depth.
    pub(super) fn resolve_user_method_or_accessor_uncached(
        &mut self,
        class_name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> Option<UserMethodOrAccessor> {
        let method_name = name_sym.as_str();
        let mro = self.class_mro(class_name);
        // The method name is interned once for the whole walk (by the caller),
        // and each level's own name is already a `Symbol` -- the `&str` probes
        // below re-interned both on every MRO level of every dispatch.
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            let (has_local_method, has_role_method, has_attr, has_native) = {
                let registry = self.registry();
                if let Some(class_def) = registry.classes.get(cn.as_str()) {
                    let (local, role) =
                        registry.user_method_local_role_presence_sym(*cn, name_sym, is_ancestor);
                    let attr = registry.accessor_is_public_sym(*cn, name_sym) == Some(true);
                    // A built-in class (e.g. Proc) may register a public attribute
                    // for `.raku`/introspection while a native method of the same
                    // name is the real getter (its computed fallbacks differ from
                    // the raw seeded default). The native method wins over the
                    // auto-generated accessor at this level.
                    let native = class_def.native_methods.contains(method_name);
                    (local, role, attr, native)
                } else if let Some(role_def) = registry.roles.get(cn.as_str()) {
                    // A punned role used as a parent class: its own methods
                    // and attribute accessors sit at this MRO level.
                    let local = role_def
                        .methods
                        .get(method_name)
                        .is_some_and(|defs| defs.iter().any(|d| !d.is_private));
                    let attr = role_def
                        .attributes
                        .iter()
                        .any(|a| a.is_public && a.name == method_name);
                    (local, false, attr, false)
                } else {
                    (false, false, false, false)
                }
            };
            if has_local_method {
                return Some(UserMethodOrAccessor::Method);
            }
            if has_attr {
                // A native getter for the same name (built-in classes only)
                // supersedes the auto-generated accessor.
                if has_native {
                    return Some(UserMethodOrAccessor::Method);
                }
                // "Class entities beat role entities" only applies when the
                // attribute really IS a class entity. When BOTH the accessor
                // and the method were contributed by composed roles
                // (`role R { has Str $.n; method n { ... } }`), the role's
                // explicit method wins — raku answers from `method n`, not from
                // the accessor. Without this, the role method became
                // unreachable as soon as the composing class had any body at
                // all (an empty body never syncs the accessor column, which is
                // why `class J does R { }` accidentally behaved correctly).
                if has_role_method && self.attribute_is_role_contributed(cn.as_str(), method_name) {
                    return Some(UserMethodOrAccessor::Method);
                }
                return Some(UserMethodOrAccessor::Accessor);
            }
            if has_role_method {
                return Some(UserMethodOrAccessor::Method);
            }
        }
        None
    }

    /// Return the declaring class of the public accessor that wins method
    /// resolution for `method_name`, or `None` when an explicit method wins or
    /// no public accessor exists. The owner is needed to find a wrap chain
    /// installed through the accessor's Method meta-object.
    // Cost: O(1) amortized (two memoized probes); a miss is O(d), d = MRO depth.
    pub(crate) fn attribute_accessor_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<crate::symbol::Symbol> {
        let name = probe_key(method_name)?;
        if !matches!(
            self.resolve_user_method_or_accessor_sym(class_name, name),
            Some(UserMethodOrAccessor::Accessor)
        ) {
            return None;
        }
        self.first_public_accessor_owner(class_name, name)
    }

    /// Whether `class_name`'s public attribute `attr_name` was contributed by a
    /// composed role rather than declared in the class body. Used to break the
    /// accessor-vs-role-method tie in
    /// [`Self::resolve_user_method_or_accessor`]: a class-declared attribute
    /// outranks a role method ("Class prioritization", 6.c
    /// `S14-roles/attributes.t`), but a role-contributed one does not.
    fn attribute_is_role_contributed(&self, class_name: &str, attr_name: &str) -> bool {
        let registry = self.registry();
        let Some(roles) = registry.class_composed_roles.get(class_name) else {
            return false;
        };
        roles.iter().any(|composed| {
            let base = composed
                .split_once('[')
                .map_or(composed.as_str(), |(base, _)| base);
            registry
                .roles
                .get(base)
                .is_some_and(|role| role.attributes.iter().any(|a| a.name == attr_name))
        })
    }

    /// Accessor-slot promotion gate: when `method_name` is a public `is rw`
    /// attribute accessor, return `Some(declared type constraint)` (`Some(None)`
    /// for an untyped rw attribute). `None` means the accessor is not rw — a
    /// want-ref read must NOT promote the slot (raku returns the decont'd value
    /// there, so `my $r := $obj.ro-attr; $r = v` stays an immutable-value error).
    pub(crate) fn rw_accessor_type_constraint(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<Option<String>> {
        let attrs = self.collect_class_attributes(class_name);
        let is_rw = attrs
            .iter()
            .any(|attr| attr.is_public && attr.is_rw && attr.name == method_name);
        if !is_rw {
            return None;
        }
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(tc) = self.get_attr_type_constraint(cn.as_str(), method_name) {
                return Some(Some(tc));
            }
        }
        Some(None)
    }

    /// Check if an attribute is buildable (can be set via .new).
    pub(crate) fn is_attribute_buildable(&self, class_name: &str, attr_name: &str) -> bool {
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if let Some(&built) = class_def.attribute_built.get(attr_name) {
                return built;
            }
            for attr in &class_def.attributes {
                if attr.name == attr_name {
                    return attr.is_public;
                }
            }
        }
        let mro = if let Some(cd) = self.registry().classes.get(class_name) {
            cd.mro.clone()
        } else {
            [].into()
        };
        for parent in mro.iter().map(|p| p.as_str()) {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry().classes.get(parent) {
                if let Some(&built) = parent_def.attribute_built.get(attr_name) {
                    return built;
                }
                for attr in &parent_def.attributes {
                    if attr.name == attr_name {
                        return attr.is_public;
                    }
                }
            }
        }
        true
    }

    /// Look up a class-level attribute (declared with `our $.x` or `my $.x`).
    /// Searches the class and its MRO.
    pub(crate) fn get_class_level_attr(&self, class_name: &str, attr_name: &str) -> Option<Value> {
        // Check own class first
        if let Some(class_def) = self.registry().classes.get(class_name)
            && let Some(val) = class_def.class_level_attrs.get(attr_name)
        {
            return Some(val.clone());
        }
        // Walk MRO for inherited class-level attributes
        let mro = self.class_mro(class_name);
        for parent in mro.iter().map(|s| s.as_str()) {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry().classes.get(parent)
                && let Some(val) = parent_def.class_level_attrs.get(attr_name)
            {
                return Some(val.clone());
            }
        }
        None
    }

    /// Check if a class (or its MRO) has a class-level attribute.
    pub(crate) fn has_class_level_attr(&self, class_name: &str, attr_name: &str) -> bool {
        self.get_class_level_attr(class_name, attr_name).is_some()
    }

    /// Set a class-level attribute value. Searches the class and its MRO to find
    /// where the attribute is defined, then updates it.
    pub(crate) fn set_class_level_attr(
        &self,
        class_name: &str,
        attr_name: &str,
        value: Value,
    ) -> bool {
        // Check own class first
        if let Some(class_def) = self.registry_mut().classes.get_mut(class_name)
            && class_def.class_level_attrs.contains_key(attr_name)
        {
            class_def
                .class_level_attrs
                .insert(attr_name.to_string(), value);
            return true;
        }
        // Walk MRO
        let mro = self.class_mro(class_name);
        for parent in mro.iter().map(|s| s.as_str()) {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry_mut().classes.get_mut(parent)
                && parent_def.class_level_attrs.contains_key(attr_name)
            {
                parent_def
                    .class_level_attrs
                    .insert(attr_name.to_string(), value);
                return true;
            }
        }
        false
    }

    /// Collect wildcard-handles attribute var names from the class and its MRO.
    pub(super) fn collect_wildcard_handles(&mut self, class_name: &str) -> Vec<String> {
        let mro = self.class_mro(class_name);
        let mut result = Vec::new();
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                result.extend(class_def.wildcard_handles.iter().cloned());
            }
        }
        result
    }

    /// Add `__mutsu_attr_alias::x` metadata for attributes declared with `has $x`
    /// (no twigil), so the method call dispatch can set up bidirectional aliases.
    pub(super) fn add_alias_attribute_metadata(&mut self, class_name: &str, attrs: &mut AttrMap) {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr_name in &class_def.alias_attributes {
                    attrs.insert(
                        MetaNs::AttrAlias.owned_key_for_str(attr_name),
                        Value::str(attr_name.to_string()),
                    );
                }
            }
        }
    }

    /// Whether `class_name` (or any class in its MRO) declares an attribute
    /// with the bare name `bare` (`"x"` for `$!x`/`$.x`). Used by the VM's
    /// private-attribute read check: a read of an attribute that the invocant's
    /// class neither carries nor declares throws (P6opaque no-such-attribute).
    pub(crate) fn class_declares_attribute(&mut self, class_name: &str, bare: &str) -> bool {
        self.class_mro(class_name).iter().any(|cn| {
            self.registry()
                .classes
                .get(cn.as_str())
                .is_some_and(|cd| cd.attributes.iter().any(|a| a.name == bare))
        })
    }

    /// The positional state every Raku `Cursor` (and hence every grammar
    /// cursor) exposes. mutsu mints a cursor as an instance of the grammar
    /// carrying these in its attribute map; the grammar itself declares none of
    /// them, so the generic accessor path consults this list before applying the
    /// "user-declared class has only its declared accessors" rule.
    pub(crate) fn is_cursor_state_attribute(name: &str) -> bool {
        matches!(name, "pos" | "orig" | "target" | "from" | "to")
    }

    pub(crate) fn collect_class_attributes(&mut self, class_name: &str) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs
                        .iter()
                        .position(|a| a.name == attr.name && a.sigil == attr.sigil)
                    {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    /// Collect a class's attributes in *introspection* order — the order
    /// `.^attributes`, `.raku` and the default `.gist` enumerate them in.
    ///
    /// This is deliberately NOT `collect_class_attributes`' order. Rakudo has
    /// two distinct attribute orders and mutsu needs both:
    ///
    /// * construction / `BUILDALL` order is base-class first (least-derived
    ///   attributes initialised before the ones that may depend on them) —
    ///   that is `collect_class_attributes`;
    /// * introspection order walks the MRO *forwards*, most-derived first,
    ///   each class's own attributes in declaration order — this function.
    ///
    /// So `class Taurus is Bull is Automobile {}` renders as
    /// `Taurus.new(castrated => Bool::False, direction => Any)`, matching
    /// `Taurus.^attributes`.
    ///
    /// A name declared by more than one class in the MRO keeps its
    /// most-derived position (rakudo lists both copies; mutsu's instance
    /// attribute map is keyed by bare name, so only one slot exists).
    pub(super) fn collect_class_attributes_display_order(
        &mut self,
        class_name: &str,
    ) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        let mut seen: HashSet<(String, char)> = HashSet::new();
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    if seen.insert((attr.name.clone(), attr.sigil)) {
                        attrs.push(attr.clone());
                    }
                }
            }
        }
        attrs
    }

    /// Collect per-class attributes for all classes in the MRO.
    /// Returns `(declaring_class, ClassAttributeDef)` pairs.
    /// Unlike `collect_class_attributes`, this does NOT deduplicate by name —
    /// if Parent and Child both declare an attribute with the same name,
    /// both entries are returned. Used to initialize class-qualified attribute
    /// storage so that each class has its own private copy.
    pub(super) fn collect_per_class_attrs(
        &mut self,
        class_name: &str,
    ) -> Vec<(String, ClassAttributeDef)> {
        let mro = self.class_mro(class_name);
        let mut result: Vec<(String, ClassAttributeDef)> = Vec::new();
        // Track which attribute names appear in multiple classes (need qualified storage)
        let mut attr_counts: HashMap<(String, char), usize> = HashMap::new();
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    *attr_counts
                        .entry((attr.name.clone(), attr.sigil))
                        .or_insert(0) += 1;
                }
            }
        }
        // Only include attrs that appear in multiple classes (duplicated across hierarchy)
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    if attr_counts
                        .get(&(attr.name.clone(), attr.sigil))
                        .copied()
                        .unwrap_or(0)
                        > 1
                    {
                        result.push((cn.resolve(), attr.clone()));
                    }
                }
            }
        }
        result
    }

    /// Collect attributes from a role and all its composed parent roles.
    /// Used when the role has been punned (instantiated via mixin) and we need
    /// to check attribute metadata (e.g. `is rw`).
    pub(super) fn collect_role_attributes_for_class(
        &self,
        role_name: &str,
    ) -> Vec<ClassAttributeDef> {
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        if let Some(role) = self.registry().roles.get(role_name) {
            attrs.extend(role.attributes.clone());
        }
        if let Some(parent_names) = self.registry().role_parents.get(role_name) {
            let mut role_stack: Vec<String> = parent_names.clone();
            let mut visited = vec![role_name.to_string()];
            while let Some(parent_role_name) = role_stack.pop() {
                if !visited.contains(&parent_role_name) {
                    visited.push(parent_role_name.clone());
                    if let Some(parent_role) = self.registry().roles.get(&parent_role_name) {
                        for attr in &parent_role.attributes {
                            if !attrs
                                .iter()
                                .any(|a| a.name == attr.name && a.sigil == attr.sigil)
                            {
                                attrs.push(attr.clone());
                            }
                        }
                    }
                    if let Some(grandparents) = self.registry().role_parents.get(&parent_role_name)
                    {
                        for gp in grandparents {
                            if !visited.contains(gp) {
                                role_stack.push(gp.clone());
                            }
                        }
                    }
                }
            }
        }
        attrs
    }
}
