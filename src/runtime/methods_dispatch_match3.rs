use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// Dispatch remaining methods by name (Supply, networking, temporal, misc).
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_3(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "Numeric" if args.is_empty() => {
                // Seq.Numeric: if backed by a PredictiveIterator, call count-only.
                // Otherwise return the element count.
                if let ValueView::Seq(items) = target.view() {
                    let seq_id = std::sync::Arc::as_ptr(items) as usize;
                    if let Some(iter) = self.predictive_seq_iter_for(seq_id) {
                        return Some(self.call_method_with_values(iter, "count-only", vec![]));
                    }
                    return Some(Ok(Value::int(items.len() as i64)));
                }
                None
            }
            "from-list" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_from_list(&args));
                }
                None
            }
            "repository-for-name" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let name = args.first().map(Value::to_string_value).unwrap_or_default();
                    if let Some(prefix) = name.strip_prefix("file#") {
                        let new_args = vec![Value::pair(
                            "prefix".to_string(),
                            Value::str(prefix.to_string()),
                        )];
                        return Some(self.call_method_with_values(
                            Value::package(Symbol::intern("CompUnit::Repository::FileSystem")),
                            "new",
                            new_args,
                        ));
                    }
                    if matches!(name.as_str(), "site" | "home" | "vendor" | "perl" | "core")
                        && let Some(dir) = Self::default_repo_dir(&name)
                    {
                        let new_args = vec![Value::pair(
                            "prefix".to_string(),
                            Value::str(dir.display().to_string()),
                        )];
                        return Some(self.call_method_with_values(
                            Value::package(Symbol::intern("CompUnit::Repository::Installation")),
                            "new",
                            new_args,
                        ));
                    }
                    return Some(Ok(Value::NIL));
                }
                None
            }
            "repository-for-spec" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let spec = args.first().map(Value::to_string_value).unwrap_or_default();
                    if let Some(prefix) = spec.strip_prefix("inst#") {
                        let new_args = vec![Value::pair(
                            "prefix".to_string(),
                            Value::str(prefix.to_string()),
                        )];
                        return Some(self.call_method_with_values(
                            Value::package(Symbol::intern("CompUnit::Repository::Installation")),
                            "new",
                            new_args,
                        ));
                    }
                    if let Some(prefix) = spec.strip_prefix("file#") {
                        let new_args = vec![Value::pair(
                            "prefix".to_string(),
                            Value::str(prefix.to_string()),
                        )];
                        return Some(self.call_method_with_values(
                            Value::package(Symbol::intern("CompUnit::Repository::FileSystem")),
                            "new",
                            new_args,
                        ));
                    }
                    return Some(Ok(Value::NIL));
                }
                None
            }
            "run-script" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let script_name = args.first().map(Value::to_string_value).unwrap_or_default();
                    return Some(self.run_script_from_repos(&script_name));
                }
                None
            }
            "signal" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_signal(&args));
                }
                None
            }
            "on-demand" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "Supply"
                {
                    return Some(self.dispatch_supply_on_demand(&args));
                }
                if let ValueView::Instance { class_name, .. } = target.view()
                    && class_name == "Supply"
                {
                    return Some(Err(RuntimeError::new(
                        "Cannot call on-demand on a Supply instance",
                    )));
                }
                None
            }
            "find" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "Encoding::Registry"
                {
                    return Some(self.dispatch_encoding_registry_find(&args));
                }
                None
            }
            "register" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "Encoding::Registry"
                {
                    return Some(self.dispatch_encoding_registry_register(&args));
                }
                None
            }
            "connect" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "IO::Socket::INET"
                {
                    return Some(self.dispatch_socket_inet_connect(&args));
                }
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_connect(&args));
                }
                None
            }
            "listen" => {
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "IO::Socket::INET"
                {
                    return Some(self.dispatch_socket_inet_listen(&args));
                }
                if let ValueView::Package(class_name) = target.view()
                    && class_name == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_listen(&args));
                }
                None
            }
            "bind-udp" => {
                if let ValueView::Package(cn) = target.view()
                    && cn == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_bind_udp(&args));
                }
                None
            }
            "udp" => {
                if let ValueView::Package(cn) = target.view()
                    && cn == "IO::Socket::Async"
                {
                    return Some(self.dispatch_socket_async_udp(&args));
                }
                None
            }
            "now" => self.dispatch_datetime_now(&target, &args),
            "Date" if args.is_empty() => {
                if let ValueView::Package(class_name) = target.view() {
                    let cn = class_name.resolve();
                    let mro = self.class_mro(&cn);
                    if mro.iter().any(|name| name == "DateTime")
                        || mro.iter().any(|name| name == "Date")
                    {
                        return Some(Ok(Value::package(Symbol::intern("Date"))));
                    }
                }
                None
            }
            "DateTime" if args.is_empty() => {
                if let ValueView::Package(class_name) = target.view() {
                    let cn = class_name.resolve();
                    let mro = self.class_mro(&cn);
                    if mro.iter().any(|name| name == "DateTime")
                        || mro.iter().any(|name| name == "Date")
                    {
                        return Some(Ok(Value::package(Symbol::intern("DateTime"))));
                    }
                }
                None
            }
            "today" if args.is_empty() => {
                if let ValueView::Package(class_name) = target.view() {
                    let cn = class_name.resolve();
                    let is_date_like =
                        cn == "Date" || self.class_mro(&cn).iter().any(|name| name == "Date");
                    if is_date_like {
                        use crate::builtins::methods_0arg::temporal;
                        // Date.today uses the local timezone ($*TZ), matching
                        // rakudo. Without the offset the date would be UTC, which
                        // differs from DateTime.now.Date in non-UTC zones.
                        let tz = self
                            .env
                            .get("*TZ")
                            .and_then(|v| {
                                if let ValueView::Int(n) = v.view() {
                                    Some(n)
                                } else {
                                    None
                                }
                            })
                            .unwrap_or(0i64);
                        let secs = crate::value::current_time_secs_f64() as i64 + tz;
                        let epoch_days = secs.div_euclid(86400);
                        let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
                        if cn == "Date" {
                            return Some(Ok(temporal::make_date(y, m, d)));
                        }
                        // For Date subclasses, construct via the subclass constructor
                        let date_args = vec![
                            Value::pair("year".to_string(), Value::int(y)),
                            Value::pair("month".to_string(), Value::int(m)),
                            Value::pair("day".to_string(), Value::int(d)),
                        ];
                        return Some(self.dispatch_new(target.clone(), date_args));
                    }
                }
                None
            }
            "Numeric" if args.is_empty() => {
                if let ValueView::Seq(items) = target.view() {
                    // Check for PredictiveIterator-backed Seq (stored by Seq.new)
                    let seq_id = std::sync::Arc::as_ptr(items) as usize;
                    if let Some(iter) = self.predictive_seq_iter_for(seq_id) {
                        // Call count-only on the PredictiveIterator
                        return Some(self.call_method_with_values(iter, "count-only", vec![]));
                    }
                    return Some(Ok(Value::int(items.len() as i64)));
                }
                None
            }
            "slice" => Some(self.dispatch_slice_method(target, args)),
            "grab" => self.dispatch_grab_method(target, args),
            "grabpairs" => self.dispatch_grabpairs_method(target, args),
            "skip" => Some(self.dispatch_skip_method(target, args)),
            "join" if args.len() <= 1 => {
                // `.join` on a Thread blocks until the thread completes and syncs
                // its shared captured variables back to the parent (Raku
                // `Thread.join`). Route it to the real thread join before the
                // generic list-`join` stringifies the Thread instance to
                // "Thread(...)" without waiting.
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                    && class_name == "Thread"
                {
                    let attrs = attributes.as_map().clone();
                    return Some(self.dispatch_thread_finish(&attrs));
                }
                self.dispatch_join_method(target, args)
            }
            "grep" => {
                // `Supply.grep` on a *live* (Supplier-backed) supply must stay
                // live: register a filter transform tap that forwards matching
                // values to a derived supply. A materialized supply
                // (`Supply.from-list`, on-demand) falls through to `dispatch_grep`
                // below, which already filters its buffered values with correct
                // smart-match semantics (grep(Int)/grep(/rx/) etc.).
                if let ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } = target.view()
                    && class_name == "Supply"
                    && crate::runtime::native_methods::supplier_id_from_attrs(&attributes.as_map())
                        .is_some()
                {
                    let matcher = args.first().cloned().unwrap_or(Value::NIL);
                    if let Some(live) = self.make_live_transform_supply(
                        &attributes.as_map(),
                        matcher,
                        crate::runtime::native_methods::TransformMode::Grep,
                    ) {
                        return Some(Ok(live));
                    }
                }
                // In Raku, `.grep` always returns a `Seq` — including over an
                // Array. `dispatch_grep` builds a `List`-kind array whose elements
                // are the matched source slots as shared `ContainerRef` cells (so a
                // writeback loop `for @a.grep(...) { $_++ }` still mutates `@a`
                // through them); wrapping those same cells in a `Seq` preserves the
                // writeback while giving the correct `Seq` type.
                Some(self.dispatch_grep(target, &args).map(|v| {
                    if let ValueView::Array(items, crate::value::ArrayKind::List) = v.view() {
                        return Value::seq(items.to_vec());
                    }
                    v
                }))
            }
            "toggle" => Some(self.dispatch_toggle(target, &args)),
            "eager" if args.is_empty() => Some(self.dispatch_eager_method(target)),
            "is-lazy" if args.is_empty() => Some(Ok(self.dispatch_is_lazy_method(&target))),
            "first" if !args.is_empty() => Some(self.dispatch_first(target, &args)),
            "first" if args.is_empty() => {
                if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                {
                    return Some(self.dispatch_first(target, &args));
                }
                // For non-Array types (e.g., Int, Str), .first returns self
                // (treating the scalar as a single-element list)
                if matches!(target.view(), ValueView::Array(..)) {
                    None // fall through to 0-arg builtin
                } else {
                    Some(Ok(target))
                }
            }
            "tree" if !args.is_empty() => Some(self.dispatch_tree(target, &args)),
            "keys" if args.is_empty() => self.dispatch_keys_method(target),
            "values" if args.is_empty() => Some(self.dispatch_values_method(target)),
            "AT-KEY" if args.len() == 1 => self.dispatch_at_key_method(&target, &args),
            "rotate" => {
                if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_supply_transform(target, "rotate", &args))
                } else {
                    Some(self.dispatch_rotate(target, &args))
                }
            }
            _ => None,
        }
    }

    /// Dispatch the "grab" method (Supply.grab, MixHash.grab).
    fn dispatch_grab_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_grab(&(attributes).as_map(), &args));
        }
        if let ValueView::Package(class_name) = target.view()
            && class_name == "Supply"
        {
            return Some(Err(RuntimeError::new(
                "Cannot call .grab on a Supply type object",
            )));
        }
        // MixHash.grab: select and remove random element(s)
        if let ValueView::Mix(mix, ..) = target.view() {
            let count = if args.is_empty() {
                1usize
            } else {
                match args[0].view() {
                    ValueView::Whatever => mix.len(),
                    _ => args[0].to_f64().max(0.0) as usize,
                }
            };
            if mix.is_empty() || count == 0 {
                return Some(Ok(Value::NIL));
            }
            use crate::builtins::rng::builtin_rand;
            let mut grabbed = Vec::new();
            let mut remaining = (**mix).clone();
            for _ in 0..count {
                if remaining.is_empty() {
                    break;
                }
                let ks: Vec<String> = remaining.keys().cloned().collect();
                let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
                let key = ks[idx].clone();
                remaining.remove(&key);
                grabbed.push(Value::str(key));
            }
            return Some(Ok(if grabbed.len() == 1 && args.is_empty() {
                grabbed.into_iter().next().unwrap()
            } else {
                Value::seq(grabbed)
            }));
        }
        // BagHash.grab: select and remove random element(s)
        if let ValueView::Bag(bag, true) = target.view() {
            return Some(self.dispatch_bag_grab(&bag.counts, &args));
        }
        None
    }

    /// Dispatch the "grabpairs" method (MixHash.grabpairs, BagHash.grabpairs).
    fn dispatch_grabpairs_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        // BagHash.grabpairs
        if let ValueView::Bag(bag, true) = target.view() {
            return Some(self.dispatch_bag_grabpairs(&bag.counts, &args));
        }
        let ValueView::Mix(mix, ..) = target.view() else {
            return None;
        };
        let count = if args.is_empty() {
            1usize
        } else {
            match args[0].view() {
                ValueView::Whatever => mix.len(),
                _ => args[0].to_f64().max(0.0) as usize,
            }
        };
        if mix.is_empty() || count == 0 {
            return Some(Ok(Value::seq(Vec::new())));
        }
        use crate::builtins::rng::builtin_rand;
        let mut grabbed = Vec::new();
        let mut remaining = (**mix).clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let ks: Vec<String> = remaining.keys().cloned().collect();
            let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
            let key = ks[idx].clone();
            let weight = remaining.remove(&key).unwrap_or(0.0);
            let weight_val = crate::value::mix_weight_to_value(weight);
            grabbed.push(Value::pair(key, weight_val));
        }
        // TODO: compile to bytecode - should mutate the original variable
        Some(Ok(Value::seq(grabbed)))
    }

    /// Helper for BagHash.grab (works on a clone, does not mutate the original).
    fn dispatch_bag_grab(
        &mut self,
        bag: &std::collections::HashMap<String, num_bigint::BigInt>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::rng::builtin_rand;
        use crate::runtime::utils::bigint_to_i128_sat;
        use num_bigint::BigInt;
        use num_traits::Signed;
        let count = if args.is_empty() {
            1usize
        } else {
            match args[0].view() {
                ValueView::Whatever => {
                    bigint_to_i128_sat(&bag.values().sum::<BigInt>()).max(0) as usize
                }
                _ => args[0].to_f64().max(0.0) as usize,
            }
        };
        if bag.is_empty() || count == 0 {
            if args.is_empty() {
                return Ok(Value::NIL);
            }
            return Ok(Value::seq(Vec::new()));
        }
        let mut grabbed = Vec::new();
        let mut remaining = bag.clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let total: i128 = remaining.values().map(bigint_to_i128_sat).sum();
            if total <= 0 {
                break;
            }
            let r = (builtin_rand() * total as f64) as i128;
            let mut cumulative = 0i128;
            let mut chosen_key = String::new();
            for (k, v) in &remaining {
                cumulative += bigint_to_i128_sat(v);
                if r < cumulative {
                    chosen_key = k.clone();
                    break;
                }
            }
            if let Some(c) = remaining.get_mut(&chosen_key) {
                *c -= BigInt::from(1);
                if !c.is_positive() {
                    remaining.remove(&chosen_key);
                }
            }
            grabbed.push(Value::str(chosen_key));
        }
        Ok(if grabbed.len() == 1 && args.is_empty() {
            grabbed.into_iter().next().unwrap()
        } else {
            Value::seq(grabbed)
        })
    }

    /// Helper for BagHash.grabpairs (works on a clone, does not mutate the original).
    fn dispatch_bag_grabpairs(
        &mut self,
        bag: &std::collections::HashMap<String, num_bigint::BigInt>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::builtins::rng::builtin_rand;
        let count = if args.is_empty() {
            1usize
        } else {
            match args[0].view() {
                ValueView::Whatever => bag.len(),
                _ => args[0].to_f64().max(0.0) as usize,
            }
        };
        if bag.is_empty() || count == 0 {
            return Ok(Value::seq(Vec::new()));
        }
        let mut grabbed = Vec::new();
        let mut remaining = bag.clone();
        for _ in 0..count {
            if remaining.is_empty() {
                break;
            }
            let ks: Vec<String> = remaining.keys().cloned().collect();
            let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
            let key = ks[idx].clone();
            let val = remaining.remove(&key).unwrap_or_default();
            grabbed.push(Value::pair(key, Value::from_bigint(val)));
        }
        Ok(Value::seq(grabbed))
    }

    /// Dispatch the "skip" method.
    fn dispatch_skip_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            return self.dispatch_supply_skip(&(attributes).as_map(), &args);
        }
        // A Seq is consumed by .skip: subsequent iteration of the original Seq
        // must throw X::Seq::Consumed (unless it was cached).
        if let ValueView::Seq(seq_items) = target.view() {
            if crate::value::seq_is_consumed(seq_items) && !crate::value::seq_is_cached(seq_items) {
                return Err(crate::value::seq_consumed_error());
            }
            if !crate::value::seq_is_cached(seq_items) {
                crate::value::seq_consume(seq_items).ok();
            }
        }
        let items = crate::runtime::utils::value_to_list(&target);
        let n = if args.is_empty() {
            1usize
        } else if matches!(args[0].view(), ValueView::Sub(..)) {
            // Callable arg: call with list length to get actual skip count
            let len = Value::int(items.len() as i64);
            let result = self.call_sub_value(args[0].clone(), vec![len], false)?;
            result.to_f64().max(0.0) as usize
        } else {
            args[0].to_f64().max(0.0) as usize
        };
        let result: Vec<Value> = items.into_iter().skip(n).collect();
        Ok(Value::seq(result))
    }

    /// Dispatch the "join" method.
    fn dispatch_join_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            target.view(),
            ValueView::Array(..)
                | ValueView::Seq(..)
                | ValueView::Slip(..)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            return None;
        }
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else if let Some(inner) = target.as_list_items() {
            inner.to_vec()
        } else {
            Self::value_to_list(&target)
        };
        let mut parts = Vec::with_capacity(items.len());
        for v in &items {
            // Decontainerize a `ContainerRef` element (grep rw alias / `:=`-bound
            // slot) so a cell-wrapped Instance still gets its user-defined `.Str`.
            let v = v.deref_container();
            if matches!(v.view(), ValueView::Instance { .. }) {
                let s = match self.call_method_with_values(v.clone(), "Str", vec![]) {
                    Ok(s) => s,
                    Err(e) => return Some(Err(e)),
                };
                parts.push(s.to_string_value());
            } else {
                parts.push(v.to_string_value());
            }
        }
        let joined = parts.join(&sep);
        Some(Ok(Value::str(joined)))
    }

    /// Dispatch the "eager" method.
    fn dispatch_eager_method(&mut self, target: Value) -> Result<Value, RuntimeError> {
        if let ValueView::LazyList(list) = target.view() {
            return Ok(Value::array(self.force_lazy_list_bridge(list)?));
        }
        if matches!(target.view(), ValueView::Array(..)) {
            let (items, kind) = target.into_array().unwrap();
            if items
                .iter()
                .any(|v| matches!(v.view(), ValueView::LazyList(_)))
            {
                // `.eager` reifies a lazy TAIL preserved inside the array (a
                // `|(lazy gather …)` slipped into `@a` sits as a `LazyList`
                // element). Force each such element so its deferred body/side
                // effects run now, flattening the produced values in place.
                let mut out = Vec::with_capacity(items.len());
                for it in items.iter() {
                    if let ValueView::LazyList(ll) = it.view() {
                        out.extend(self.force_lazy_list_bridge(ll)?);
                    } else {
                        out.push(it.clone());
                    }
                }
                return Ok(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(out)),
                    kind,
                ));
            }
            return Ok(Value::array_with_kind(items, kind));
        }
        if matches!(target.view(), ValueView::Seq(..) | ValueView::Slip(..)) {
            Ok(target)
        } else if matches!(
            target.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            Ok(Value::array(crate::runtime::utils::value_to_list(&target)))
        } else {
            // A plain scalar interprets itself as a single-element List.
            Ok(Value::array(vec![target]))
        }
    }

    /// Dispatch the "is-lazy" method.
    fn dispatch_is_lazy_method(&self, target: &Value) -> Value {
        let value_is_lazy = |v: &Value| match v.view() {
            ValueView::LazyList(_) => true,
            ValueView::Array(_, kind) if kind.is_lazy() => true,
            ValueView::Range(_, end)
            | ValueView::RangeExcl(_, end)
            | ValueView::RangeExclStart(_, end)
            | ValueView::RangeExclBoth(_, end) => end == i64::MAX,
            ValueView::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            _ => false,
        };
        let is_lazy = if value_is_lazy(target) {
            true
        } else if let Some(items) = target.as_list_items() {
            items.iter().any(value_is_lazy)
        } else {
            false
        };
        Value::truth(is_lazy)
    }

    /// Dispatch "keys" method.
    fn dispatch_keys_method(&mut self, target: Value) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let keys = match attributes.as_map().get("symbols").map(Value::view) {
                    Some(ValueView::Hash(map)) => {
                        map.keys().cloned().map(Value::str).collect::<Vec<Value>>()
                    }
                    _ => Vec::new(),
                };
                Some(Ok(Value::seq(keys)))
            }
            ValueView::Hash(map) => {
                if let Some(info) = self.container_type_metadata(&target)
                    && info.key_type.is_some()
                {
                    // Object hash: use the original key objects embedded in the
                    // hash's `HashData` (single source, COW-stable).
                    if let Some(ref orig) = utils::hash_original_keys_snapshot(&target)
                        && !orig.is_empty()
                    {
                        let keys: Vec<Value> = map
                            .keys()
                            .map(|k| utils::hash_typed_key(&target, k))
                            .collect();
                        return Some(Ok(Value::seq(keys)));
                    }
                    // Fallback: try coercion from string key
                    let key_constraint = info.key_type.unwrap();
                    let mut keys = Vec::with_capacity(map.len());
                    for key in map.keys() {
                        let key_value = Value::str(key.clone());
                        let coerced = self
                            .try_coerce_value_for_constraint(&key_constraint, key_value)
                            .unwrap_or_else(|_| Value::str(key.clone()));
                        keys.push(coerced);
                    }
                    return Some(Ok(Value::seq(keys)));
                }
                let keys = map.keys().cloned().map(Value::str).collect::<Vec<Value>>();
                Some(Ok(Value::seq(keys)))
            }
            ValueView::Mixin(inner, _) if matches!(inner.as_ref().view(), ValueView::Hash(_)) => {
                Some(self.call_method_with_values(inner.as_ref().clone(), "keys", vec![]))
            }
            _ => None,
        }
    }

    /// Dispatch "values" method.
    fn dispatch_values_method(&self, target: Value) -> Result<Value, RuntimeError> {
        match target.view() {
            ValueView::Capture { positional, named } => {
                let mut vals = positional.clone();
                vals.extend(named.values().cloned());
                Ok(Value::seq(vals))
            }
            ValueView::Array(items, ..) => Ok(Value::seq(items.to_vec())),
            ValueView::Hash(map) => Ok(Value::seq(map.values().cloned().collect())),
            ValueView::Pair(_, value) => Ok(Value::seq(vec![value.clone()])),
            ValueView::ValuePair(_, value) => Ok(Value::seq(vec![value.clone()])),
            _ => Ok(Value::seq(Vec::new())),
        }
    }

    /// Dispatch "AT-KEY" method.
    fn dispatch_at_key_method(
        &self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match (target.view(), &args[0]) {
            (
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                },
                idx,
            ) if class_name == "Stash" => {
                if let Some(ValueView::Hash(symbols)) =
                    attributes.as_map().get("symbols").map(Value::view)
                {
                    let stash_lookup = |raw_key: &str| {
                        if let Some(value) = symbols.get(raw_key) {
                            return Some(value.clone());
                        }
                        if !raw_key.starts_with('$')
                            && !raw_key.starts_with('@')
                            && !raw_key.starts_with('%')
                            && !raw_key.starts_with('&')
                        {
                            let scalar = format!("${raw_key}");
                            if let Some(value) = symbols.get(&scalar) {
                                return Some(value.clone());
                            }
                        }
                        None
                    };

                    if let ValueView::Array(items, ..) = idx.view() {
                        let values = items
                            .iter()
                            .map(|item| {
                                let key = item.to_string_value();
                                stash_lookup(&key).unwrap_or(Value::NIL)
                            })
                            .collect::<Vec<_>>();
                        return Some(Ok(Value::array(values)));
                    }

                    let key = idx.to_string_value();
                    if let Some(value) = stash_lookup(&key) {
                        return Some(Ok(value));
                    }
                }
                Some(Ok(Value::NIL))
            }
            (ValueView::Pair(key, value), idx) => {
                if key == &idx.to_string_value() {
                    Some(Ok(value.clone()))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            (ValueView::ValuePair(key, value), idx) => {
                if key.to_string_value() == idx.to_string_value() {
                    Some(Ok(value.clone()))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            // `$match.AT-KEY("name")` — named-capture lookup, mirroring the
            // `$match<name>` postcircumfix. Routed here by hyper method calls such
            // as `@<chars>».<spaces>`, which dispatch `.AT-KEY` per element.
            (
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                },
                idx,
            ) if class_name == "Match" => {
                let key = idx.to_string_value();
                if let Some(ValueView::Hash(named)) =
                    attributes.as_map().get("named").map(Value::view)
                {
                    Some(Ok(named.get(key.as_str()).cloned().unwrap_or(Value::NIL)))
                } else {
                    Some(Ok(Value::NIL))
                }
            }
            _ => None,
        }
    }

    /// Dispatch the "slice" method on Seq/Array/List.
    /// Returns a Seq of elements at the given indices.
    /// With no args, returns an empty Seq.
    pub(crate) fn dispatch_slice_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let items = crate::runtime::utils::value_to_list(&target);
        if args.is_empty() {
            return Ok(Value::seq(Vec::new()));
        }
        // Collect all indices from args (flattening lists/arrays)
        let mut indices: Vec<usize> = Vec::new();
        for arg in &args {
            match arg.view() {
                ValueView::Int(i) => {
                    if i >= 0 {
                        indices.push(i as usize);
                    }
                }
                ValueView::Array(elems, _) => {
                    for elem in elems.iter() {
                        let i = elem.to_f64() as i64;
                        if i >= 0 {
                            indices.push(i as usize);
                        }
                    }
                }
                ValueView::Seq(elems) | ValueView::Slip(elems) => {
                    for elem in elems.iter() {
                        let i = elem.to_f64() as i64;
                        if i >= 0 {
                            indices.push(i as usize);
                        }
                    }
                }
                _ => {
                    let i = arg.to_f64() as i64;
                    if i >= 0 {
                        indices.push(i as usize);
                    }
                }
            }
        }
        let result: Vec<Value> = indices
            .into_iter()
            .map(|i| items.get(i).cloned().unwrap_or(Value::NIL))
            .collect();
        Ok(Value::seq(result))
    }
}
