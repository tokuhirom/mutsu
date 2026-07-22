use super::*;

impl Interpreter {
    pub(super) fn try_native_method(
        &mut self,
        target: &Value,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        fn collection_contains_instance(value: &Value) -> bool {
            // Recursive: does any element (transitively) carry an Instance whose
            // `.gist` may need interpreter dispatch?
            fn contains_instance(value: &Value) -> bool {
                match value.view() {
                    ValueView::Instance { .. } => true,
                    _ if value.as_list_items().is_some() => {
                        value.as_list_items().unwrap().iter().any(contains_instance)
                    }
                    ValueView::Hash(map) => map.values().any(contains_instance),
                    _ => false,
                }
            }
            // Only a *collection* receiver triggers the gist bypass. A bare
            // instance (e.g. a `Buf`, whose gist `native_method_0arg` renders
            // purely via `dispatch_core_repr`) is dispatched normally — the
            // builtins layer itself defers a collection whose elements may have a
            // user `method gist`, so bypassing a bare instance here only forced a
            // pure native gist (Buf/Blob/Uni) onto the interpreter for nothing.
            match value.view() {
                _ if value.as_list_items().is_some() => {
                    value.as_list_items().unwrap().iter().any(contains_instance)
                }
                ValueView::Hash(map) => map.values().any(contains_instance),
                _ => false,
            }
        }
        let method_name = method_sym.resolve();
        // A `Seq.new($iterator)` stores its iterator deferred (empty backing vec).
        // The native impls below read that empty vec directly and would yield
        // nothing (`.List`/`.elems`/`.Array`/...). Defer such a Seq to the
        // interpreter, whose `call_method_with_values` pulls every element from
        // the iterator (`materialize_deferred_seq`) before dispatching. `cache`/
        // `raku`/`perl` intentionally keep the Seq lazy (they must NOT pull), so
        // they proceed with the native path.
        if let ValueView::Seq(items) = target.view()
            && crate::value::seq_has_deferred_iter(&items)
            && !crate::value::seq_deferred_method_keeps_lazy(method_name.as_str())
        {
            return None;
        }
        // `.Capture` that must call user methods / drain a live source (Channel/
        // Supply, or a non-Str Pair key needing `.Str`) is interpreter-aware; other
        // targets fall through to the pure native `value_to_capture` below.
        if method_name == "Capture"
            && args.is_empty()
            && let Some(r) = self.try_interpreter_capture(target)
        {
            return Some(r);
        }
        // A `.map`/`.grep` on a lazy source that the interpreter would turn into
        // a lazy pipeline stage (a chained map/grep pipe, an infinite sequence, or
        // a genuinely-lazy `gather { … }.lazy`) must be deferred to the interpreter
        // (`dispatch_map_method`/`dispatch_grep` via `is_lazy_pipe_source`). Running
        // the native impl here would materialize the source eagerly — forcing the
        // whole gather body (and its trailing side effects) instead of pulling on
        // demand. Defer.
        if let ValueView::LazyList(_) = target.view()
            && Self::is_lazy_pipe_source(target)
            && matches!(method_name.as_str(), "map" | "grep")
        {
            return None;
        }
        // A laziness-preserving coercion on a lazy map/grep pipeline returns the
        // pipeline unchanged (it stays pullable). Intercept before the native
        // impl, which would otherwise wrap/empty the pipeline's (empty) cache.
        if let ValueView::LazyList(ll) = target.view()
            && ll.lazy_pipe.is_some()
            && Self::lazy_pipe_preserving_coercion(method_name.as_str())
        {
            return Some(Ok(target.clone()));
        }
        // Eager list operations cannot run on a lazy/infinite source: throw
        // X::Cannot::Lazy instead of hanging while the native impl materializes
        // it (matches raku). Shared with the interpreter dispatch path.
        if let Some(err) =
            crate::runtime::Interpreter::lazy_guard_error(method_name.as_str(), target)
        {
            return Some(Err(err));
        }
        // Early exit for Proxy containers
        if matches!(target.view(), ValueView::Proxy { .. })
            && !matches!(
                method_name.as_str(),
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            )
        {
            return None;
        }
        // Squish/tail always bypass native
        if method_sym == "squish" || method_sym == "tail" {
            return None;
        }
        // Buf write methods (`write-int*`/`write-uint*`/`write-num*`) on a type object
        // (`buf8.write-int32($off, $val, [$endian])` → fresh buf) or a non-mut-bound
        // instance (`\sigilless`/`(buf8.new).write-...` → mutate the shared cell). Pure
        // value / VM-owned shared-cell mutation, no interpreter state. Handle natively
        // (2-or-3-arg form, which the arity-keyed native_method_*arg dispatch below
        // cannot cover for 3 args). Blob and bad arity fall through to the interpreter.
        if let Some(result) =
            crate::builtins::buf_write_int::try_native_buf_write(target, &method_name, args)
        {
            return Some(result);
        }
        // Mixin role method bypass
        if self.mixin_role_has_method(target, &method_name) {
            return None;
        }
        // Instance-specific bypasses (avoid for non-Instance targets)
        if matches!(target.view(), ValueView::Instance { .. }) {
            if let ValueView::Instance { class_name, .. } = target.view() {
                let cn = class_name.resolve();
                // Supply methods
                if cn == "Supply"
                    && matches!(
                        method_name.as_str(),
                        "max"
                            | "min"
                            | "lines"
                            | "elems"
                            | "head"
                            | "flat"
                            | "sort"
                            | "comb"
                            | "words"
                            | "batch"
                            | "throttle"
                            | "rotor"
                            | "rotate"
                            | "produce"
                            | "snip"
                            | "minmax"
                            | "start"
                            | "wait"
                            | "zip"
                            | "zip-latest"
                    )
                {
                    return None;
                }
                // Supplier.Supply
                if cn == "Supplier" && method_sym == "Supply" && args.is_empty() {
                    return None;
                }
                // Numeric bridge — Real/Numeric instances route through the
                // interpreter's Numeric bridge for arithmetic and coercion.
                // Exception: pure-render methods (gist/Str/raku/...) don't need
                // the bridge, so let `native_method_0arg` render the known
                // builtin numeric instances (Instant/Duration) directly. A
                // user numeric class that overrides one of these keeps the
                // bypass so its method runs; unknown numeric instances return
                // `None` from native and still fall through to the interpreter.
                let is_pure_render = matches!(
                    method_name.as_str(),
                    "gist" | "Str" | "Stringy" | "raku" | "perl"
                );
                let render_overridden = is_pure_render && self.has_user_method(&cn, &method_name);
                if (!is_pure_render || render_overridden)
                    && (self.type_matches_value("Real", target)
                        || self.type_matches_value("Numeric", target)
                        || self.has_user_method(&cn, "Bridge"))
                {
                    return None;
                }
                // Native method on instance class
                if self.is_native_method(&cn, &method_name) {
                    return None;
                }
            }
        } else if matches!(target.view(), ValueView::Package(name) if name == "Supply")
            && matches!(
                method_name.as_str(),
                "max"
                    | "min"
                    | "lines"
                    | "elems"
                    | "head"
                    | "flat"
                    | "sort"
                    | "comb"
                    | "words"
                    | "batch"
                    | "throttle"
                    | "rotor"
                    | "rotate"
                    | "produce"
                    | "snip"
                    | "minmax"
                    | "start"
                    | "wait"
                    | "zip"
                    | "zip-latest"
            )
        {
            return None;
        }
        // Collection gist bypass
        if method_sym == "gist" && args.is_empty() && collection_contains_instance(target) {
            return None;
        }
        // Collection .raku/.perl bypass: an element whose `.raku` is only
        // reachable through method dispatch (a user instance, a built-in object
        // type) must be rendered by the interpreter — the pure renderer would
        // fall back to `Foo()` (see `runtime::methods_raku_dispatch`).
        if matches!(method_name.as_str(), "raku" | "perl")
            && args.is_empty()
            && crate::runtime::container_needs_raku_dispatch(target)
        {
            return None;
        }
        // Pick/roll on Package/Str
        if (method_sym == "pick" || method_sym == "roll")
            && args.len() <= 1
            && matches!(target.view(), ValueView::Package(_) | ValueView::Str(_))
        {
            return None;
        }
        // Hash-specific bypasses
        if matches!(target.view(), ValueView::Hash(_)) && args.is_empty() {
            let mn = method_name.as_str();
            if (mn == "raku" || mn == "perl" || mn == "keyof")
                && self.container_type_metadata(target).is_some()
            {
                return None;
            }
            if mn == "keyof" {
                return None;
            }
        }
        // Typed array .raku/.perl bypass
        if matches!(target.view(), ValueView::Array(..))
            && args.is_empty()
            && matches!(method_name.as_str(), "raku" | "perl")
            && self
                .container_type_metadata(target)
                .is_some_and(|info| info.value_type != "Any" && info.value_type != "Mu")
        {
            return None;
        }
        // `.contains($needle, $pos?, :i/:m?)` — the positioned / case-insensitive
        // forms carry a Pair or a 3rd arg, so they never reach the arity-keyed
        // native_method_*arg dispatch below. Handle them natively (pure Str search,
        // mirrors Interpreter::dispatch_contains). The bare single-needle form keeps
        // its native_method_1arg arm; out-of-range / BigInt positions fall through.
        if method_name == "contains"
            && let Some(result) = crate::builtins::native_contains_with_options(target, args)
        {
            return Some(result);
        }
        // `.starts-with`/`.ends-with`/`.substr-eq` with `:i`/`:ignorecase` markings
        // (and, for substr-eq, a start position) carry a Pair / 3rd arg that pushes
        // them past the arity-keyed dispatch below. Handle the case-insensitive forms
        // natively (mirror dispatch_prefix_suffix_check / dispatch_substr_eq); the bare
        // forms keep their 1-/2-arg arms, and `:m`/out-of-range fall through.
        if matches!(method_name.as_str(), "starts-with" | "ends-with")
            && let Some(result) = crate::builtins::native_prefix_suffix_with_options(
                target,
                args,
                method_name == "starts-with",
            )
        {
            return Some(result);
        }
        if method_name == "substr-eq"
            && let Some(result) = crate::builtins::native_substr_eq_with_options(target, args)
        {
            return Some(result);
        }
        let mut result = if args.len() == 2 {
            crate::builtins::native_method_2arg(target, method_sym, &args[0], &args[1])
        } else if args.len() == 1 {
            crate::builtins::native_method_1arg(target, method_sym, &args[0])
        } else if args.is_empty() {
            crate::builtins::native_method_0arg(target, method_sym)
        } else {
            return None;
        };

        // For gather-based LazyList, .List and .values preserve laziness
        // by returning the LazyList itself (which can be forced on demand).
        if result.is_none()
            && let ValueView::LazyList(ll) = target.view()
            && ll.coroutine.is_some()
            && matches!(method_name.as_str(), "List" | "values")
        {
            return Some(Ok(target.clone()));
        }

        // Handle .Slip/.List/.Seq on scan-based LazyList by forcing elements via Interpreter.
        if result.is_none()
            && let ValueView::LazyList(ll) = target.view()
            && ll.scan_spec.is_some()
            && matches!(method_name.as_str(), "Slip" | "List" | "Seq" | "Array")
        {
            match self.force_lazy_list_vm(&ll) {
                Ok(items) => {
                    let val = match method_name.as_str() {
                        "Slip" => Value::slip(items),
                        "List" => Value::array(items),
                        "Seq" => Value::seq(items),
                        "Array" => Value::real_array(items),
                        _ => unreachable!(),
                    };
                    return Some(Ok(val));
                }
                Err(e) => return Some(Err(e)),
            }
        }

        if method_name == "decode" {
            return result.map(|res| {
                res.map(|value| match value.view() {
                    ValueView::Str(decoded) => {
                        Value::str(self.translate_newlines_for_decode(&decoded))
                    }
                    _ => value,
                })
            });
        }

        // For Hash values with declared_type "Map", override gist/raku/perl
        // to use Map.new((...)) format instead of {...} format.
        if let ValueView::Hash(map) = target.view() {
            let is_map = matches!(method_name.as_str(), "gist" | "raku" | "perl")
                && self
                    .container_type_metadata(target)
                    .and_then(|info| info.declared_type)
                    .is_some_and(|dt| dt == "Map");
            if is_map {
                if map.is_empty() {
                    return Some(Ok(Value::str("Map.new".to_string())));
                }
                let mut sorted_keys: Vec<&String> = map.keys().collect();
                sorted_keys.sort();
                if method_name == "gist" {
                    let total = sorted_keys.len();
                    let truncated = total > 100;
                    let display_keys = if truncated {
                        &sorted_keys[..100]
                    } else {
                        &sorted_keys[..]
                    };
                    let mut parts: Vec<String> = display_keys
                        .iter()
                        .map(|k| format!("{} => {}", k, crate::runtime::gist_value(&map[*k])))
                        .collect();
                    if truncated {
                        parts.push("...".to_string());
                    }
                    return Some(Ok(Value::str(format!("Map.new(({}))", parts.join(", ")))));
                } else {
                    let parts: Vec<String> = sorted_keys
                        .iter()
                        .map(|k| {
                            let v = &map[*k];
                            if let ValueView::Bool(true) = v.view() {
                                format!(":{}", k)
                            } else if let ValueView::Bool(false) = v.view() {
                                format!(":!{}", k)
                            } else {
                                let repr = if matches!(v.view(), ValueView::Nil) {
                                    "Any".to_string()
                                } else {
                                    crate::builtins::methods_0arg::raku_value(v)
                                };
                                format!(":{}({})", k, repr)
                            }
                        })
                        .collect();
                    return Some(Ok(Value::str(format!("Map.new(({}))", parts.join(", ")))));
                }
            }
        }

        // When cloning a container (Hash/Array/Set/Bag/Mix) that has type
        // metadata, copy the metadata to the cloned value so the clone
        // preserves the type constraint (e.g. %h.clone ~~ Hash[Int,Int]).
        if method_name == "clone"
            && matches!(&result, Some(Ok(_)))
            && let Some(info) = self.container_type_metadata(target)
            && let Some(Ok(v)) = result.take()
        {
            // Hashes embed metadata in `HashData`; re-tag the cloned value
            // (no-op Arc for array/instance side-table containers).
            result = Some(Ok(self.tag_container_metadata(v, info)));
        }

        result
    }

    pub(super) fn try_native_function(
        &mut self,
        name_sym: crate::symbol::Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        // Atomic var/element RMW markers (`__mutsu_atomic_*` / `__mutsu_cas_*`) for the
        // `⚛`-operators. Dispatch them here directly so they don't fall through to the
        // generic `call_function` name-match (§D state ownership). Checked before the
        // Instance-arg bail below because a cas/store value may legitimately be an
        // Instance (e.g. `cas($x, $old, $obj)`).
        {
            let name = name_sym.resolve();
            if (name.starts_with("__mutsu_atomic_") || name.starts_with("__mutsu_cas_"))
                && let Some(result) = self.try_native_atomic_function(name.as_str(), args)
            {
                return Some(result);
            }
        }
        if args
            .iter()
            .any(|arg| matches!(arg.view(), ValueView::Instance { .. }))
        {
            // Junction constructors wrap their arguments verbatim regardless of
            // type, so an Instance argument is safe to handle natively; every
            // other native function bails out on Instance args (they may need
            // method dispatch) and falls through to the interpreter.
            let name = name_sym.resolve();
            // `unpack(Blob, Str)` takes a Blob (Instance) argument and is
            // handled natively; everything else bails out on Instance args.
            if !matches!(name.as_str(), "any" | "all" | "one" | "none" | "unpack") {
                return None;
            }
        }
        // For functions that need to iterate their arguments (e.g. flat),
        // force any LazyList arguments first so the pure builtin can process them.
        if args
            .iter()
            .any(|a| matches!(a.view(), ValueView::LazyList(_)))
        {
            let name = name_sym.resolve();
            if matches!(name.as_str(), "flat" | "eager") {
                // For `flat`, a single lazy argument stays lazy (`flat 42 xx *`
                // / `flat 10,11 ... *` propagate `.is-lazy`). The one exception
                // is a finite `gather` coroutine: it must be forced and
                // flattened here so `flat` descends into its nested lazy elements
                // (`take internals($child)` in a binary-tree walk), which a raw
                // return would leave un-descended (`('A', Seq(...), ...)`). Only
                // a coroutine-backed, not-yet-forced gather is force-flattened;
                // every other lazy list keeps its laziness.
                // TODO: an actually-infinite `gather` (`flat gather { loop {
                // take $i++ } }`) is forced eagerly here and would hang — raku
                // keeps it lazy. (A `lazy gather` takes the flattening-pipe
                // path below and stays lazy.)
                let is_finite_gather = |v: &Value| {
                    matches!(v.view(), ValueView::LazyList(ll)
                        if ll.coroutine.is_some() && ll.cache.lock().unwrap().is_none())
                };
                if name.as_str() == "flat"
                    && args.len() == 1
                    && let ValueView::LazyList(ll) = args[0].view()
                    && !is_finite_gather(&args[0])
                {
                    // A genuinely-lazy list may hold nested arrays
                    // (`flat [2,3,4], 10, 11 ... *`), so wrap it in a lazy
                    // flattening pipe that pulls source elements on demand and
                    // spills each through `flat_val` — flattened AND still lazy.
                    if ll.is_genuinely_lazy() {
                        return Some(Ok(Value::lazy_list(crate::gc::Gc::new(
                            crate::value::LazyList::new_index_pipe(
                                args[0].clone(),
                                crate::value::IndexTransform::Flat,
                            ),
                        ))));
                    }
                    return Some(Ok(args[0].clone()));
                }
                let mut forced_args = Vec::with_capacity(args.len());
                for arg in args {
                    if let ValueView::LazyList(ll) = arg.view() {
                        match self.force_lazy_list_vm(&ll) {
                            Ok(items) => {
                                forced_args.push(Value::seq(items));
                            }
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        forced_args.push(arg.clone());
                    }
                }
                return crate::builtins::native_function(name_sym, &forced_args);
            }
        }
        crate::builtins::native_function(name_sym, args)
    }
}
