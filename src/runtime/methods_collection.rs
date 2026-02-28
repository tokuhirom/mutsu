use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        match target {
            Value::Set(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.clone());
                            }
                        }
                        _ => {
                            elems.insert(item.to_string_value());
                        }
                    }
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Bag(b) => {
                for k in b.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Mix(m) => {
                for k in m.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Pair(k, v) => {
                if v.truthy() {
                    elems.insert(k);
                }
            }
            other => {
                elems.insert(other.to_string_value());
            }
        }
        Ok(Value::set(elems))
    }

    pub(super) fn dispatch_to_bag(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        match target {
            Value::Bag(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let c = match v.as_ref() {
                                Value::Int(i) => *i,
                                Value::Num(n) => *n as i64,
                                _ => 1,
                            };
                            *counts.entry(k.clone()).or_insert(0) += c;
                        }
                        _ => {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    counts.insert(k.clone(), 1);
                }
            }
            Value::Mix(m) => {
                for (k, v) in m.iter() {
                    counts.insert(k.clone(), *v as i64);
                }
            }
            Value::Hash(h) => {
                for (k, v) in h.iter() {
                    let c = match v {
                        Value::Int(i) => *i,
                        Value::Num(n) => *n as i64,
                        Value::Bool(b) => i64::from(*b),
                        _ => i64::from(v.truthy()),
                    };
                    if c > 0 {
                        counts.insert(k.clone(), c);
                    }
                }
            }
            Value::Pair(k, v) => {
                let c = match v.as_ref() {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    _ => 1,
                };
                if c > 0 {
                    counts.insert(k, c);
                }
            }
            other => {
                counts.insert(other.to_string_value(), 1);
            }
        }
        Ok(Value::bag(counts))
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        match target {
            Value::Mix(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.clone()).or_insert(0.0) += w;
                        }
                        Value::ValuePair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.to_string_value()).or_insert(0.0) += w;
                        }
                        _ => {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    weights.insert(k.clone(), 1.0);
                }
            }
            Value::Bag(b) => {
                for (k, v) in b.iter() {
                    weights.insert(k.clone(), *v as f64);
                }
            }
            Value::Pair(k, v) => {
                let w = match v.as_ref() {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                    _ => 1.0,
                };
                *weights.entry(k.clone()).or_insert(0.0) += w;
            }
            Value::ValuePair(k, v) => {
                let w = match v.as_ref() {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                    _ => 1.0,
                };
                *weights.entry(k.to_string_value()).or_insert(0.0) += w;
            }
            Value::Hash(h) => {
                for (k, v) in h.iter() {
                    let w = match v {
                        Value::Int(i) => *i as f64,
                        Value::Num(n) => *n,
                        Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                        Value::Bool(b) => {
                            if *b {
                                1.0
                            } else {
                                0.0
                            }
                        }
                        _ => {
                            if v.truthy() {
                                1.0
                            } else {
                                0.0
                            }
                        }
                    };
                    if w != 0.0 {
                        weights.insert(k.clone(), w);
                    }
                }
            }
            other => {
                weights.insert(other.to_string_value(), 1.0);
            }
        }
        Ok(Value::mix(weights))
    }

    pub(super) fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        match target {
            Value::Hash(_) => Ok(target),
            Value::Array(items, ..) => {
                let mut map = HashMap::new();
                let mut iter = items.iter();
                while let Some(item) = iter.next() {
                    match item {
                        Value::Pair(k, v) => {
                            map.insert(k.clone(), *v.clone());
                        }
                        Value::ValuePair(k, v) => {
                            map.insert(k.to_string_value(), *v.clone());
                        }
                        other => {
                            let key = other.to_string_value();
                            let value = iter.next().cloned().unwrap_or(Value::Nil);
                            map.insert(key, value);
                        }
                    }
                }
                Ok(Value::hash(map))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut map = HashMap::new();
                let mut iter = items.iter();
                while let Some(item) = iter.next() {
                    match item {
                        Value::Pair(k, v) => {
                            map.insert(k.clone(), *v.clone());
                        }
                        Value::ValuePair(k, v) => {
                            map.insert(k.to_string_value(), *v.clone());
                        }
                        other => {
                            let key = other.to_string_value();
                            let value = iter.next().cloned().unwrap_or(Value::Nil);
                            map.insert(key, value);
                        }
                    }
                }
                Ok(Value::hash(map))
            }
            Value::Set(s) => {
                let mut map = HashMap::new();
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                }
                Ok(Value::hash(map))
            }
            Value::Bag(b) => {
                let mut map = HashMap::new();
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Mix(m) => {
                let mut map = HashMap::new();
                for (k, v) in m.iter() {
                    map.insert(k.clone(), Value::Num(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Instance {
                ref class_name,
                ref attributes,
                ..
            } if class_name == "Match" => {
                // %($/) returns the named captures hash
                if let Some(named) = attributes.get("named") {
                    Ok(named.clone())
                } else {
                    Ok(Value::hash(HashMap::new()))
                }
            }
            other => {
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
    }
}
