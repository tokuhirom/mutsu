use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn build_native_strdistance_value(args: &[Value]) -> Value {
        let mut before = String::new();
        let mut after = String::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "before" => before = value.to_string_value(),
                    "after" => after = value.to_string_value(),
                    _ => {}
                }
            }
        }
        let mut attrs = HashMap::new();
        attrs.insert("before".to_string(), Value::str(before));
        attrs.insert("after".to_string(), Value::str(after));
        Value::make_instance(Symbol::intern("StrDistance"), attrs)
    }

    /// VM-native construction for `FakeScheduler.new` — pure data: a fresh
    /// process-global scheduler id seeded at virtual time 0.0. `next_fake_scheduler_id`
    /// / `fake_scheduler_init` only touch a process-global table (no env / registry /
    /// user code), so the VM builds it directly, byte-identical to the interpreter.
    pub(crate) fn build_native_fakescheduler_value() -> Value {
        let sched_id = super::native_methods::next_fake_scheduler_id();
        super::native_methods::fake_scheduler_init(sched_id, 0.0);
        let mut attrs = HashMap::new();
        attrs.insert("scheduler_id".to_string(), Value::Int(sched_id as i64));
        Value::make_instance(Symbol::intern("FakeScheduler"), attrs)
    }

    /// VM-native construction for `Proxy.new(:FETCH(...), :STORE(...))` — pure data
    /// assembly that wraps the (already-evaluated) FETCH/STORE callables.
    pub(crate) fn build_native_proxy_value(args: &[Value]) -> Value {
        let mut fetcher = Value::Nil;
        let mut storer = Value::Nil;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "FETCH" => fetcher = (**value).clone(),
                    "STORE" => storer = (**value).clone(),
                    _ => {}
                }
            }
        }
        Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: None,
            decontainerized: false,
        }
    }

    /// VM-native construction for `Match.new(:orig, :from, :pos|:to, :list, :hash)` —
    /// pure data assembly: the matched substring is sliced out of `orig[from..to]`
    /// and the positional/named captures stored as instance attributes.
    pub(crate) fn build_native_match_value(args: &[Value]) -> Value {
        let mut orig = String::new();
        let mut from: i64 = 0;
        let mut to: i64 = 0;
        let mut list = Value::array(Vec::new());
        let mut hash = Value::hash(HashMap::new());
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "orig" => orig = value.to_string_value(),
                    "from" => from = to_int(value),
                    "pos" | "to" => to = to_int(value),
                    "list" => list = (**value).clone(),
                    "hash" => hash = (**value).clone(),
                    _ => {}
                }
            }
        }
        // Compute matched string from orig[from..to]
        let matched: String = orig
            .chars()
            .skip(from as usize)
            .take((to - from) as usize)
            .collect();
        let mut attrs = HashMap::new();
        attrs.insert("str".to_string(), Value::str(matched));
        attrs.insert("from".to_string(), Value::Int(from));
        attrs.insert("to".to_string(), Value::Int(to));
        attrs.insert("orig".to_string(), Value::str(orig));
        // Convert list to positional captures
        if let Value::Array(items, ..) = &list {
            attrs.insert("list".to_string(), Value::array(items.to_vec()));
        } else {
            attrs.insert("list".to_string(), Value::array(Vec::new()));
        }
        // Convert hash (Map) to named captures
        if let Value::Hash(map, ..) = &hash {
            attrs.insert("named".to_string(), Value::hash(map.as_ref().clone()));
        } else {
            attrs.insert("named".to_string(), Value::hash(HashMap::new()));
        }
        Value::make_instance(Symbol::intern("Match"), attrs)
    }

    /// VM-native construction for an allomorph type (`IntStr`/`NumStr`/`RatStr`/
    /// `ComplexStr`) — `.new(numeric, string)` is pure data assembly: the inner
    /// numeric value (unwrapped from an allomorphic `Mixin` argument) is mixed
    /// with a `Str` override carrying the string form. No env / registry / user
    /// code. The interpreter's `dispatch_new_and_constructors` arm calls the same
    /// helper, so the native path is byte-identical.
    pub(crate) fn build_native_allomorph_value(
        type_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(format!(
                "{}.new requires two arguments (numeric, string)",
                type_name
            )));
        }
        // Unwrap allomorphic (Mixin) arguments to get the inner numeric value.
        let numeric = match &args[0] {
            Value::Mixin(inner, _) => (**inner).clone(),
            other => other.clone(),
        };
        let string = args[1].to_string_value();
        let mut mixins = HashMap::new();
        mixins.insert("Str".to_string(), Value::str(string));
        Ok(Value::mixin(numeric, mixins))
    }

    /// VM-native construction for `ObjAt`/`ValueObjAt` — `.new(which)` is pure
    /// data assembly: the first positional argument's stringification is stored
    /// as the `WHICH` attribute. A missing positional is the same arity error the
    /// interpreter raises. Shared with the interpreter's
    /// `dispatch_new_and_constructors` arm so the native path is byte-identical.
    pub(crate) fn build_native_objat_value(
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let positional = args
            .iter()
            .find(|a| !matches!(a, Value::Pair(_, _) | Value::ValuePair(_, _)));
        match positional {
            Some(val) => {
                let mut attrs = HashMap::new();
                attrs.insert("WHICH".to_string(), Value::str(val.to_string_value()));
                Ok(Value::make_instance(class_name, attrs))
            }
            None => Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1".to_string(),
            )),
        }
    }

    /// VM-native construction for `Failure.new($exception?)` — pure data assembly
    /// reading only VM-owned state: the explicit exception argument (or `$!` from
    /// env when omitted, or the `X::AdHoc("Failed")` default), wrapped into an
    /// `X::AdHoc` if it is not already an `Exception`/`X::`/`CX::` (checked via the
    /// MRO read `mro_readonly`). No FS / process / user code. The interpreter's
    /// `dispatch_new_and_constructors` arm calls the same helper, so the native
    /// path is byte-identical (the VM and interpreter are one struct, so `self.env`
    /// — and thus `$!` — is identical at the call site).
    /// VM-native construction for `Seq.new($iterator?)`. Construction reads and
    /// writes only VM-owned state: a `PredictiveIterator` argument is stashed in
    /// the `predictive_seq_iters` carrier table (plus an `__mutsu_*` env side
    /// table so the association survives sub/block returns), a materialized
    /// `items`/`stuff` instance is copied eagerly, any other iterator is
    /// registered as a deferred (lazy) iterator keyed off the new Seq's own Arc,
    /// and the no-arg form yields a pre-consumed Seq. No FS / process / user
    /// code — pulling from the iterator happens later, on consumption. The
    /// interpreter's `dispatch_new` arm delegates here, so the native VM fast
    /// path is byte-identical (one struct, one `self`).
    pub(crate) fn try_native_seq_construct(&mut self, args: &[Value]) -> Value {
        // Seq.new(iterator)
        if let Some(iterator) = args.first() {
            if matches!(iterator, Value::Instance { .. })
                && self.type_matches_value("PredictiveIterator", iterator)
            {
                let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
                if let Value::Seq(items) = &seq {
                    let seq_id = std::sync::Arc::as_ptr(items) as usize;
                    // Store off the scoped env so the association
                    // survives sub/block returns (see field docs).
                    self.predictive_seq_iters.insert(seq_id, iterator.clone());
                    self.env.insert(
                        format!("__mutsu_predictive_seq_iter::{seq_id}"),
                        iterator.clone(),
                    );
                }
                return seq;
            }
            if let Value::Instance { attributes, .. } = iterator {
                let map = attributes.as_map();
                if let Some(Value::Array(items, ..)) = map.get("items").or_else(|| map.get("stuff"))
                {
                    return Value::Seq(std::sync::Arc::new(items.to_vec()));
                }
            }
            // Register deferred iterator: don't pull eagerly.
            // Raku's Seq.new(iterator) creates a lazy Seq; pulling
            // happens only when the Seq is actually consumed/iterated.
            let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
            if let Value::Seq(items) = &seq {
                crate::value::seq_register_deferred_iter(items, iterator.clone());
            }
            return seq;
        }
        // Seq.new() with no args creates a pre-consumed Seq.
        // This matches Raku: Seq.new() has no iterator, so it's
        // immediately consumed. This is what .raku returns for
        // consumed Seqs ("Seq.new()") so the EVAL roundtrip works.
        let seq = Value::Seq(std::sync::Arc::new(Vec::new()));
        if let Value::Seq(items) = &seq {
            let _ = crate::value::seq_consume(items);
        }
        seq
    }

    pub(crate) fn build_native_failure_value(&self, args: &[Value]) -> Value {
        let default_exception = || {
            let mut attrs = HashMap::new();
            // `Failure.new` with no explicit exception defaults to X::AdHoc in
            // Raku, not the abstract base Exception.
            attrs.insert("message".to_string(), Value::str("Failed".to_string()));
            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
        };
        let raw_exception = args
            .first()
            .cloned()
            .filter(|v| !matches!(v, Value::Nil))
            .or_else(|| {
                self.env
                    .get("!")
                    .cloned()
                    .filter(|v| !matches!(v, Value::Nil))
            })
            .unwrap_or_else(default_exception);
        // Wrap non-Exception values in X::AdHoc (Raku behavior).
        let wrap_adhoc = |raw: Value| {
            let mut attrs = HashMap::new();
            attrs.insert("payload".to_string(), raw.clone());
            attrs.insert("message".to_string(), Value::str(raw.to_string_value()));
            Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
        };
        let exception = if let Value::Instance { class_name, .. } = &raw_exception {
            let cn = class_name.resolve();
            if cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .mro_readonly(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"))
            {
                raw_exception
            } else {
                wrap_adhoc(raw_exception)
            }
        } else {
            wrap_adhoc(raw_exception)
        };
        let mut attrs = HashMap::new();
        attrs.insert("exception".to_string(), exception);
        attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance(Symbol::intern("Failure"), attrs)
    }
}
