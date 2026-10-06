//! The metadata of a `native`-declared type (rakudo's `NativeHOW`).
//!
//! `native long is Int is ctype<long> is unsigned is repr<P6int> { }` is how
//! rakudo's `NativeCall::Types` declares its C integer types (ADR-11203,
//! #11204). The three traits are not parents: rakudo's core `trait_mod:<is>`
//! turns `is ctype<...>`, `is nativesize(...)` and `is unsigned` into
//! `.^set_ctype` / `.^set_nativesize` / `.^set_unsigned` calls on the type's
//! HOW, which only `NativeHOW` answers. What they record is what `.REPR`,
//! `.^nativesize` and `.^unsigned` report afterwards.

use super::*;
use crate::opcode::DeclTraitArg;

/// The traits a `native` declaration recorded.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct NativeDecl {
    /// `is repr<...>` (`P6int`, `P6num`, `P6str`); `None` when not given.
    pub(crate) repr: Option<String>,
    /// `.^nativesize`: bits for `is nativesize(N)`, or MoarVM's negative
    /// C-type code for `is ctype<...>`. `None` when neither was given.
    pub(crate) nativesize: Option<i64>,
    /// `.^unsigned`.
    pub(crate) unsigned: bool,
}

impl NativeDecl {
    /// The core native type with this declaration's C layout, for the
    /// marshalling layer, which keys on core names: `uint64` for upstream's
    /// `native size_t is ctype<size_t> is unsigned is repr<P6int>`. `None`
    /// for a layout no core native has (`longdouble`).
    // Cost: O(1).
    pub(crate) fn core_native_name(&self) -> Option<&'static str> {
        match self.repr.as_deref()? {
            "P6int" => {
                let bits = match self.nativesize? {
                    // `bool` is C's `_Bool`, which the marshaller keys on its
                    // own name (one signed byte).
                    -7 => return Some("bool"),
                    8 | -1 => 8,
                    16 | -2 => 16,
                    32 | -3 => 32,
                    64 | -4 | -5 | -6 | -8 => 64,
                    _ => return None,
                };
                Some(match (bits, self.unsigned) {
                    (8, false) => "int8",
                    (8, true) => "uint8",
                    (16, false) => "int16",
                    (16, true) => "uint16",
                    (32, false) => "int32",
                    (32, true) => "uint32",
                    (_, false) => "int64",
                    (_, true) => "uint64",
                })
            }
            "P6num" => match self.nativesize? {
                32 | -1 => Some("num32"),
                64 | -2 => Some("num64"),
                _ => None,
            },
            "P6str" => Some("str"),
            _ => None,
        }
    }
}

/// The traits [`Interpreter::apply_native_type_traits`] owns. They are
/// handled here and never dispatched to a user `trait_mod:<is>`.
pub(crate) fn is_native_type_trait(name: &str) -> bool {
    matches!(name, "ctype" | "nativesize" | "unsigned")
}

/// MoarVM's code for a C type, as `.^nativesize` reports it after
/// `is ctype<...>`: `MVM_P6INT_C_TYPE_*` for an integer REPR, and
/// `MVM_P6NUM_C_TYPE_*` for `P6num`.
// Cost: O(1).
fn ctype_nativesize(ctype: &str, is_num: bool) -> Option<i64> {
    Some(if is_num {
        match ctype {
            "float" => -1,
            "double" => -2,
            "longdouble" => -3,
            _ => return None,
        }
    } else {
        match ctype {
            "char" => -1,
            "short" => -2,
            "int" => -3,
            "long" => -4,
            "longlong" => -5,
            "size_t" => -6,
            "bool" => -7,
            "atomic" => -8,
            _ => return None,
        }
    })
}

impl Interpreter {
    /// Record a type's `is ctype` / `is nativesize` / `is unsigned` traits
    /// and, for a `native` declaration, its REPR. A class (or any non-native
    /// declarator) carrying one of those traits fails as rakudo's does: the
    /// trait calls a setter that only `NativeHOW` has.
    // Cost: O(t), t = traits on the declaration.
    pub(crate) fn apply_native_type_traits(
        &mut self,
        type_name: &str,
        repr: Option<&str>,
        traits: &[(String, Option<DeclTraitArg>)],
    ) -> Result<(), RuntimeError> {
        let is_native = traits.iter().any(|(t, _)| t == "__mutsu_native_decl");
        let mut decl = NativeDecl {
            repr: repr.map(str::to_string),
            ..NativeDecl::default()
        };
        let is_num = repr == Some("P6num");
        for (name, arg) in traits {
            if !is_native_type_trait(name) {
                continue;
            }
            if !is_native {
                return Err(RuntimeError::new(format!(
                    "No such method 'set_{name}' for invocant of type 'Perl6::Metamodel::ClassHOW'"
                )));
            }
            let value = match arg {
                Some(arg) => self.eval_decl_trait_arg(arg)?,
                None => Value::TRUE,
            };
            match name.as_str() {
                "unsigned" => decl.unsigned = value.truthy(),
                "nativesize" => decl.nativesize = Some(crate::runtime::to_int(&value)),
                _ => {
                    let ctype = value.to_string_value();
                    decl.nativesize = Some(ctype_nativesize(&ctype, is_num).ok_or_else(|| {
                        RuntimeError::new(format!("Unknown ctype '{ctype}' for {type_name}"))
                    })?);
                }
            }
        }
        if is_native {
            self.registry_mut()
                .native_decls
                .insert(type_name.to_string(), decl);
        }
        Ok(())
    }

    /// The traits a `native` declaration of `type_name` recorded, if any.
    // Cost: O(r), r = chars of the recorded REPR name (cloned).
    pub(crate) fn native_decl(&self, type_name: &str) -> Option<NativeDecl> {
        self.registry().native_decls.get(type_name).cloned()
    }

    /// Resolve a parameter constraint to its core native integer layout, if
    /// it names either a core native type or a compatible native declaration.
    // Cost: O(n) average, n = chars in the constraint (the registry lookup hashes it).
    pub(crate) fn native_int_binding_type<'a>(
        &self,
        constraint: &'a str,
    ) -> Option<&'a str> {
        let (base, _) = crate::runtime::types::strip_type_smiley(constraint);
        if crate::runtime::native_types::is_native_int_type(base) {
            return Some(base);
        }
        self.registry()
            .native_decls
            .get(base)
            .and_then(|decl| decl.core_native_name())
    }
}

/// `.REPR` of the type object of a built-in buffer name. `buf8`, `blob16`, ...
/// are the aliases of the parameterized roles (`Buf[uint8]`), so they are
/// `Uninstantiable`; `utf8`/`utf16`/`utf32` are classes over a `VMArray`.
/// NativeCall's `validnctype` accepts a `utf8` parameter by that REPR.
// Cost: O(1).
pub(crate) fn builtin_buf_type_repr(type_name: &str) -> Option<&'static str> {
    match type_name {
        "buf8" | "buf16" | "buf32" | "buf64" | "blob8" | "blob16" | "blob32" | "blob64" => {
            Some("Uninstantiable")
        }
        "utf8" | "utf16" | "utf32" => Some("VMArray"),
        _ => None,
    }
}

/// `.REPR` of a built-in native type object: `int*`/`uint*`/`byte`/
/// `atomicint` are `P6int`, `num*` is `P6num`, `str` is `P6str`.
// Cost: O(1).
pub(crate) fn builtin_native_repr(type_name: &str) -> Option<&'static str> {
    // The core's own native types only: `long`, `size_t` and the other C
    // aliases are `NativeCall::Types` declarations and answer through
    // [`Interpreter::native_decl`].
    if matches!(
        type_name,
        "int"
            | "int8"
            | "int16"
            | "int32"
            | "int64"
            | "uint"
            | "uint8"
            | "uint16"
            | "uint32"
            | "uint64"
            | "byte"
            | "atomicint"
    ) {
        Some("P6int")
    } else if matches!(type_name, "num" | "num32" | "num64") {
        Some("P6num")
    } else if type_name == "str" {
        Some("P6str")
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ctype_codes_match_moarvm() {
        assert_eq!(ctype_nativesize("long", false), Some(-4));
        assert_eq!(ctype_nativesize("size_t", false), Some(-6));
        assert_eq!(ctype_nativesize("bool", false), Some(-7));
        assert_eq!(ctype_nativesize("double", true), Some(-2));
        assert_eq!(ctype_nativesize("long", true), None);
    }

    #[test]
    fn builtin_reprs() {
        assert_eq!(builtin_native_repr("int32"), Some("P6int"));
        assert_eq!(builtin_native_repr("uint"), Some("P6int"));
        assert_eq!(builtin_native_repr("num"), Some("P6num"));
        assert_eq!(builtin_native_repr("str"), Some("P6str"));
        assert_eq!(builtin_native_repr("Int"), None);
        assert_eq!(builtin_native_repr("long"), None);
    }
}
