use super::*;

impl Interpreter {
    /// Coerce a value bound to a `%`-sigil target (variable or attribute) to a
    /// Hash, list-contextualizing a non-Hash object the way Raku's `Hash.STORE`
    /// does. An object with a custom `.iterator`/`.list` (e.g. delegated via
    /// `handles <iterator list>`) contributes its pairs, so `my %h = $obj` /
    /// `has %.x` bound to such an object materializes those pairs into the hash
    /// instead of degrading to a single stringified key.
    ///
    /// A plain object without a custom list interface has `.list` == `(self,)`,
    /// which coerces to the same scalar fallback as the object itself, so this
    /// is safe for any Instance. `Match` keeps its dedicated `%(...)` handling
    /// in `coerce_to_hash`.
    pub(crate) fn coerce_object_to_hash(&mut self, value: Value) -> Value {
        // A role mixed into a Hash is still an associative value. Coercing the
        // wrapper itself would stringify its raku representation into one
        // bogus key; assignment to a % variable must see the wrapped Hash's
        // entries instead.
        if let ValueView::Mixin(inner, _) = value.view()
            && matches!(inner.view(), ValueView::Hash(_))
        {
            return crate::runtime::utils::coerce_to_hash(inner.as_ref().clone());
        }
        if let ValueView::Instance { .. } = value.view()
            && !value.is_match_instance()
            && let Ok(listed) = self.call_method_with_values(value.clone(), "list", Vec::new())
        {
            return crate::runtime::utils::coerce_to_hash(listed);
        }
        // A lazy sequence (e.g. `%h = gather { take ... }` or `%h = ....map(...)`)
        // must be reified before it can be split into key/value pairs. Without
        // forcing, `coerce_to_hash` falls to its `_` arm and stringifies the whole
        // unreified LazyList into a single bogus key. Force it into an eager Seq
        // first, matching Raku (assignment to a `%` container is eager).
        if let ValueView::LazyList(ll) = value.view()
            && let Ok(items) = self.force_lazy_list(&ll)
        {
            return crate::runtime::utils::coerce_to_hash(Value::seq(items));
        }
        crate::runtime::utils::coerce_to_hash(value)
    }

    /// [`coerce_attr_value_by_sigil`](Self::coerce_attr_value_by_sigil) for a
    /// value **supplied by the caller** (`C.new(x => @src)`, `self.bless(|%args)`).
    ///
    /// An `@`/`%` attribute IS a container and the object owns it: Raku assigns
    /// the supplied list's elements INTO the attribute's own container, so a
    /// later mutation through the attribute (`$o.x.push(9)`, `$o.x = (…)`,
    /// `$o.h<k> = …`) can never reach the caller's `@src`. Sharing the supplied
    /// `Gc` made every one of those write straight through to it.
    ///
    /// Only the *provided-argument* sites use this; a default expression
    /// (`has @.x = 1,2`) is re-evaluated per instance and is already the
    /// object's own container.
    pub(crate) fn coerce_provided_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        let coerced = Self::coerce_attr_value_by_sigil(val, sigil);
        if matches!(sigil, '@' | '%')
            && let Some(owned) = coerced.detached_container_copy()
        {
            return owned;
        }
        coerced
    }

    /// Coerce a value based on attribute sigil: @ → Array, % → Hash
    pub(crate) fn coerce_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        match sigil {
            // Raku assigns to an `@`-sigil attribute exactly the way `my @a = …`
            // assigns, so this arm asks the ONE list-assignment rule
            // (`coerce_to_array`) rather than re-deriving a partial copy of it:
            // `Positional`/iterable values flatten, a `Hash` flattens to its
            // pairs, `Nil` becomes `[Any]`, and a plain scalar or type object
            // becomes a one-element `Array`. The hand-written arms this replaces
            // covered `Array`/`Range`/`Seq` but fell through with `val.clone()`
            // for everything else, so `C.new(a => 5)` stored a bare `Int` in an
            // `@` attribute and `has @.w = 1..3` (whose parse-time wrap this
            // change also drops) stored the Range as one element.
            '@' => match val.view() {
                // A genuinely deferred `Seq` (`Seq.new($iterator)`,
                // `IO::Handle.lines`, `(1..Inf).map(…)`) passes through
                // unmaterialized: this function has no `&mut Interpreter` to
                // force one with, and an infinite source must not be reified
                // here. The first read still consumes it correctly; only a
                // repeat read stays wrong, exactly as before.
                ValueView::Seq(items) if items.has_deferred_source() => val.clone(),
                _ => crate::runtime::utils::coerce_to_array(val),
            },
            '%' => match val.view() {
                ValueView::Hash(_) => val.clone(),
                ValueView::Pair(k, v) => {
                    // A single Pair coerces to a one-element hash
                    let mut map = HashMap::new();
                    map.insert(k.clone(), v.clone());
                    Value::hash(map)
                }
                // A general-key Pair (`"A" => "b"` builds ValuePair, not the
                // interned-Str-key Pair variant) coerces the same way, with the
                // hash-key stringification `build_hash_from_items` uses.
                ValueView::ValuePair(k, v) => {
                    let mut map = HashMap::new();
                    map.insert(Value::hash_key_encode(k), v.clone());
                    Value::hash(map)
                }
                // A list coerces to a Hash exactly like `my %h = list`: Pairs
                // flatten, bare elements pair up `key => value`, and an *empty*
                // list yields an empty Hash (previously a no-pair / empty list
                // was kept as an Array, so `%!attr<k>` then died "does not
                // support associative indexing" — surfaced by HTTP::MediaType's
                // `has %.parameters` when a media type carried no parameters).
                // An odd non-pair count raku-throws "Odd number of elements";
                // `coerce_attr_value_by_sigil` cannot throw, so keep the raw
                // value on that error and let a later type check report it.
                ValueView::Array(arr, _) => {
                    match crate::runtime::utils::build_hash_from_items(arr.to_vec()) {
                        Ok(h) => h,
                        Err(_) => val.clone(),
                    }
                }
                ValueView::Slip(items) => {
                    match crate::runtime::utils::build_hash_from_items(items.to_vec()) {
                        Ok(h) => h,
                        Err(_) => val.clone(),
                    }
                }
                _ => val.clone(),
            },
            _ => val,
        }
    }

    pub(super) fn assumed_signature_param_defs(
        data: &crate::value::SubData,
        assumed_positional: &[Value],
        assumed_named: &std::collections::HashMap<String, Value>,
    ) -> Option<Vec<ParamDef>> {
        if data.param_defs.is_empty() {
            return None;
        }
        // Owned: this builds the *primed* signature of an `.assuming` wrapper
        // by editing the copy, so it cannot share the `SubData`'s `Arc`.
        let mut param_defs = data.param_defs.to_vec();
        // Build type capture mappings from assumed positional args
        let mut type_captures: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        // Helper to check if an assumed value is a Whatever placeholder
        let is_placeholder = |v: &Value| {
            matches!(v.view(), ValueView::Whatever)
                || matches!(v.view(), ValueView::Num(f) if f.is_infinite())
                || matches!(v.view(), ValueView::Rat(_, 0))
        };
        {
            let mut pos_idx = 0usize;
            for pd in &param_defs {
                if !pd.named && !pd.slurpy {
                    if pos_idx < assumed_positional.len() {
                        if !is_placeholder(&assumed_positional[pos_idx])
                            && let Some(tc) = &pd.type_constraint
                            && let Some(capture_name) = tc.strip_prefix("::")
                        {
                            let resolved_type = crate::runtime::utils::value_type_name(
                                &assumed_positional[pos_idx],
                            )
                            .to_string();
                            type_captures.insert(capture_name.to_string(), resolved_type);
                        }
                        pos_idx += 1;
                    } else {
                        break;
                    }
                }
            }
        }
        // Remove params that have been primed (non-placeholder assumed values).
        // Whatever (*) placeholders leave the corresponding param in the signature.
        let mut assumed_iter = assumed_positional.iter();
        param_defs.retain(|pd| {
            if !pd.named
                && !pd.slurpy
                && let Some(assumed) = assumed_iter.next()
            {
                // Placeholder (*) means keep this param in the signature
                return is_placeholder(assumed);
            }
            true
        });
        // Apply type capture resolution to remaining params
        if !type_captures.is_empty() {
            for pd in &mut param_defs {
                if let Some(tc) = &pd.type_constraint
                    && let Some(resolved) = type_captures.get(tc.as_str())
                {
                    pd.type_constraint = Some(resolved.clone());
                }
            }
        }
        // When .assuming() binds a named argument, that parameter is shown in
        // the primed signature with the bound value as its default (and becomes
        // optional, dropping any `!`). Named parameters that were NOT primed
        // keep their original state, including their own defaults. A named
        // parameter can be bound by any of its alias names (`:b(:c($a))` binds
        // to either `b` or `c`).
        for pd in &mut param_defs {
            if pd.named
                && let Some(value) = assumed_named_binding(pd, assumed_named)
            {
                pd.default = Some(crate::ast::Expr::Literal(value));
                pd.required = false;
                pd.optional_marker = false;
                pd.where_constraint = None;
            }
        }
        Some(param_defs)
    }
}

/// Returns the bound value if `.assuming` primed this named parameter, matching
/// on the parameter's primary name or any of its alias names (`:b(:c($a))` can
/// be bound as either `b` or `c`). Mirrors `collect_named_names`: only nested
/// named sub-signature params contribute alias names.
fn assumed_named_binding(
    pd: &ParamDef,
    assumed_named: &std::collections::HashMap<String, Value>,
) -> Option<Value> {
    fn strip_sigil(name: &str) -> &str {
        name.strip_prefix(['@', '%', '&']).unwrap_or(name)
    }
    if let Some(v) = assumed_named.get(strip_sigil(&pd.name)) {
        return Some(v.clone());
    }
    let mut cur = pd;
    while cur.named
        && let Some(subs) = &cur.sub_signature
        && subs.len() == 1
        && subs[0].named
    {
        cur = &subs[0];
        if let Some(v) = assumed_named.get(strip_sigil(&cur.name)) {
            return Some(v.clone());
        }
    }
    None
}
