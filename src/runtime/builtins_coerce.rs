use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The container-type coercion calls `Array(...)` / `List(...)` / `Hash(...)`.
    /// Each argument becomes one element (Raku does not deep-flatten these:
    /// `Array((1,2), 3).elems` is 2), so the args list is materialized directly.
    /// A lone argument instead coerces like the method form (`List(x)` is
    /// `x.List`), per the single-argument rule.
    /// A single type-object argument (`Array(Int)`) is a parametric type request
    /// rather than a value coercion, so it passes through as a `Type(Type)`
    /// package rendering, mirroring [`Self::builtin_coerce`].
    // Cost: O(n), n = elements of the result (one argument: as its `.List` / `.Array`).
    pub(super) fn builtin_container_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // `Array(Int)` etc.: a lone type-object argument is a parametric type,
        // not a value list. Render it like the scalar coercions do.
        if args.len() == 1
            && let ValueView::Package(sym) = args[0].view()
        {
            return Ok(Value::package(Symbol::intern(&format!(
                "{name}({})",
                sym.resolve()
            ))));
        }
        // A lone argument follows the single-argument rule: `List(x)` and
        // `Array(x)` coerce it like the method form (`x.List` / `x.Array`), so
        // an Iterable -- itemized or not -- contributes its elements
        // (`List((1, 2))` is `(1 2)`, #11299).
        // A value already of the target type is returned as is, like any
        // coercion type (`List([1, 2])` stays the Array `[1 2]`).
        if args.len() == 1 && matches!(name, "Array" | "List") {
            if self.type_matches_value(name, args[0].descalarize()) {
                return Ok(args[0].descalarize().clone());
            }
            return self.call_method_with_values(args[0].clone(), name, vec![]);
        }
        // `Hash(...)` consumes its arguments in list context, so an Array or
        // List supplied as the sole argument contributes its elements. This
        // is what makes `Hash(@pairs)` and `Hash(do for ...)` useful for the
        // common list-of-Pairs construction. Several `Array(...)` / `List(...)`
        // arguments keep one element each (`Array(1, (2, 3))` is `[1 (2 3)]`).
        let items: Vec<Value> = if name == "Hash" {
            args.iter().flat_map(Self::value_to_list).collect()
        } else {
            args.to_vec()
        };
        Ok(match name {
            "Array" => Value::real_array(items),
            "List" => Value::array(items),
            "Hash" => self.build_hash_from_items_warning(items)?,
            _ => Value::real_array(items),
        })
    }

    /// Build a Map from call-position arguments. Unlike Hash(...), the
    /// immutable map constructor keeps its values unitemized, so share the
    /// same flattening and metadata-aware implementation as Map.new(...).
    pub(crate) fn builtin_map_coerce(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::package(Symbol::intern("Map(Any)")));
        }
        if args.len() == 1
            && let ValueView::Package(sym) = args[0].view()
        {
            return Ok(Value::package(Symbol::intern(&format!(
                "Map({})",
                sym.resolve()
            ))));
        }
        self.try_native_hash_construct(Symbol::intern("Map"), &None, args)
    }

    // Cost: O(1) dispatch plus the delegated method's cost (`Int(x)` is `x.Int`).
    pub(super) fn builtin_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Some(value) = args.first().cloned() else {
            // `Int()` with no args is a coercion type term `Int(Any)`, not a call.
            return Ok(Value::package(Symbol::intern(&format!("{name}(Any)"))));
        };
        if let Some(source) = match value.view() {
            ValueView::Package(sym) => Some(sym.resolve()),
            ValueView::Nil => Some("Any".to_string()),
            _ => None,
        } {
            return Ok(Value::package(Symbol::intern(&format!("{name}({source})"))));
        }
        if matches!(
            name,
            "Buf"
                | "Blob"
                | "buf8"
                | "buf16"
                | "buf32"
                | "buf64"
                | "blob8"
                | "blob16"
                | "blob32"
                | "blob64"
        ) {
            return Ok(Self::build_native_buf_value(Symbol::intern(name), args));
        }
        let coerced = match name {
            // `Rat($x)` / `FatRat($x)` / `Complex($x)` delegate to the method
            // form (`$x.Rat` …), which carries the full native coercion logic
            // incl. Failure semantics for invalid strings (JSON::Unmarshal's
            // `multi _unmarshal(Any:D $json, Rat) { Rat($json) }`).
            // `Real($x)` / `Numeric($x)` likewise delegate to the method form
            // (`$x.Real` / `$x.Numeric`), which handles every numeric variant
            // and string parsing with Failure semantics.
            //
            // `Int($x)` / `Num($x)` do the same, so a List or Seq numifies to
            // its element count (`Int((1, 2))` is 2, #11299).
            // A value already of the target type is returned unchanged, as a
            // coercion type does (`Int(True)` is `True`).
            "Int" | "Num" | "Rat" | "FatRat" | "Complex" | "Real" | "Numeric" => {
                if self.type_matches_value(name, &value) {
                    return Ok(value);
                }
                return self.call_method_with_values(value, name, vec![]);
            }
            "Str" => self.call_method_with_values(value, "Str", vec![])?,
            "Bool" => Value::truth(value.truthy()),
            "Uni" => {
                // Uni(codepoint) creates a Uni from a single codepoint value
                let cp = match value.view() {
                    ValueView::Int(i) => i as u32,
                    ValueView::Num(f) => f as u32,
                    _ => value.to_string_value().parse::<u32>().unwrap_or(0),
                };
                let text: String = char::from_u32(cp).into_iter().collect();
                Value::uni(String::new(), text)
            }
            _ => Value::NIL,
        };
        Ok(coerced)
    }
}
