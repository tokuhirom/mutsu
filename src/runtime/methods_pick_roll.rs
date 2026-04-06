use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch enum type-object pick/roll methods.
    pub(super) fn dispatch_enum_pick_roll(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "pick" | "roll") || args.len() > 1 {
            return None;
        }
        let enum_type_name: Option<String> = match target {
            Value::Package(type_name) => Some(type_name.resolve()),
            Value::Str(type_name) if self.enum_types.contains_key(type_name.as_str()) => {
                Some(type_name.to_string())
            }
            Value::Mixin(_, mixins) => mixins.values().find_map(|v| match v {
                Value::Enum { enum_type, .. }
                    if self.enum_types.contains_key(&enum_type.resolve()) =>
                {
                    Some(enum_type.resolve())
                }
                _ => None,
            }),
            _ => None,
        };
        let type_name = enum_type_name?;
        let pool: Option<Vec<Value>> = if type_name == "Bool" {
            Some(vec![Value::Bool(false), Value::Bool(true)])
        } else {
            self.enum_types.get(&type_name).map(|variants| {
                variants
                    .iter()
                    .enumerate()
                    .map(|(index, (key, value))| Value::Enum {
                        enum_type: Symbol::intern(&type_name),
                        key: Symbol::intern(key),
                        value: value.clone(),
                        index,
                    })
                    .collect()
            })
        };
        let pool = pool?;
        Some(Self::pick_roll_from_pool(method, args, pool))
    }

    /// Dispatch string pick/roll (character-wise).
    pub(super) fn dispatch_string_pick_roll(
        &self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let Value::Str(s) = target else {
            return None;
        };
        if !matches!(method, "pick" | "roll") || args.len() > 1 {
            return None;
        }
        let chars: Vec<Value> = s.chars().map(|c| Value::str(c.to_string())).collect();
        if chars.is_empty() {
            return Some(Ok(if args.is_empty() {
                Value::Nil
            } else {
                Value::array(Vec::new())
            }));
        }
        Some(Self::pick_roll_from_pool(method, args, chars))
    }

    /// Pick or roll from a pool of values.
    pub(super) fn pick_roll_from_pool(
        method: &str,
        args: &[Value],
        pool: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if pool.is_empty() {
            return Ok(Value::Nil);
        }
        if args.is_empty() {
            let idx =
                (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize % pool.len();
            return Ok(pool[idx].clone());
        }
        let count = match &args[0] {
            Value::Int(i) if *i > 0 => Some(*i as usize),
            Value::Int(_) => Some(0),
            Value::Num(f) if f.is_nan() => {
                return Err(RuntimeError::new("Cannot convert NaN to Int"));
            }
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
            Value::Num(f) if *f < 0.0 => Some(0),
            Value::Num(f) => Some(*f as usize),
            Value::Whatever => None,
            Value::Str(s) => s.trim().parse::<i64>().ok().map(|n| n.max(0) as usize),
            _ => None,
        };
        let Some(count) = count else {
            if method == "pick" {
                let mut items = pool.clone();
                let len = items.len();
                for i in (1..len).rev() {
                    let j =
                        (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize % (i + 1);
                    items.swap(i, j);
                }
                return Ok(Value::array(items));
            }
            let generated = 1024usize;
            let mut out = Vec::with_capacity(generated);
            for _ in 0..generated {
                let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                    % pool.len();
                out.push(pool[idx].clone());
            }
            return Ok(Value::LazyList(std::sync::Arc::new(
                crate::value::LazyList::new_cached(out),
            )));
        };
        if method == "pick" {
            if count == 0 {
                return Ok(Value::array(Vec::new()));
            }
            let mut items = pool.clone();
            let mut out = Vec::with_capacity(count.min(items.len()));
            for _ in 0..count.min(items.len()) {
                let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                    % items.len();
                out.push(items.swap_remove(idx));
            }
            return Ok(Value::array(out));
        }
        if count == 0 {
            return Ok(Value::array(Vec::new()));
        }
        if count == 1 {
            let idx =
                (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize % pool.len();
            return Ok(pool[idx].clone());
        }
        let mut out = Vec::with_capacity(count);
        for _ in 0..count {
            let idx =
                (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize % pool.len();
            out.push(pool[idx].clone());
        }
        Ok(Value::array(out))
    }
}
