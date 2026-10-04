//! Type constraints `C[T]` where `C` declares its own `method ^parameterize`.
//!
//! Rakudo compiles a parameter or variable typed `C[T]` by evaluating `C[T]`
//! once: a call to `C.HOW.parameterize(C, T)`, which a class can supply as
//! `method ^parameterize`. Upstream NativeCall's `CArray` does
//! (`CArray[int32]` is `CArray.^mixin(IntTypedCArray[int32])`), so an
//! `is native` routine's `CArray[T]` parameter is such a constraint.
//!
//! mutsu keeps a type constraint as its spelling, and the generic checker can
//! only compare that spelling against names. Here the spelling is turned into
//! the type object it denotes -- by the same meta-method call `C[T]` makes as
//! an expression -- and the value is smartmatched against that, which is what
//! `$x ~~ C[T]` already does.

use super::*;

/// Split `C[A, B]` into `C` and its top-level arguments. `None` for anything
/// that is not a single bracketed parameterization.
// Cost: O(n), n = chars of the constraint.
fn split_parameterization(constraint: &str) -> Option<(&str, Vec<&str>)> {
    let open = constraint.find('[')?;
    let base = constraint[..open].trim();
    let inner = constraint[open + 1..].strip_suffix(']')?;
    if base.is_empty() {
        return None;
    }
    let mut args = Vec::new();
    let (mut depth, mut start) = (0usize, 0usize);
    for (i, c) in inner.char_indices() {
        match c {
            '[' | '(' => depth += 1,
            ']' | ')' => depth = depth.checked_sub(1)?,
            ',' if depth == 0 => {
                args.push(inner[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    args.push(inner[start..].trim());
    if args.iter().any(|a| a.is_empty()) {
        return None;
    }
    Some((base, args))
}

impl Interpreter {
    /// Whether `value` satisfies the constraint `C[T, ...]`, for a `C` that
    /// declares `method ^parameterize`. `None` when the constraint is not of
    /// that kind (a role, a built-in container, an unknown name), leaving the
    /// generic checker's answer in place.
    // Cost: O(n) to split the spelling, n = its chars; the meta-method runs
    // once per spelling (cached), then one smartmatch per check.
    pub(crate) fn meta_parameterized_match(
        &mut self,
        constraint: &str,
        value: &Value,
    ) -> Option<bool> {
        let ty = self.meta_parameterized_type(constraint)?;
        Some(self.smart_match(value, &ty))
    }

    /// The type object the constraint `C[T, ...]` denotes, for a `C` that
    /// declares `method ^parameterize`; `None` for any other spelling. The
    /// meta-method runs once per spelling and the result is cached, so a
    /// later [`Self::cached_meta_parameterized_type`] can answer without
    /// running code.
    // Cost: O(n) to split the spelling, n = its chars; the meta-method runs
    // once per spelling (cached).
    pub(crate) fn meta_parameterized_type(&mut self, constraint: &str) -> Option<Value> {
        if let Some(ty) = self.caches.meta_parameterized_types.get(constraint) {
            return Some(ty.clone());
        }
        let ty = self.eval_meta_parameterized(constraint)?;
        self.caches
            .meta_parameterized_types
            .insert(constraint.to_string(), ty.clone());
        Some(ty)
    }

    /// The cached result of [`Self::meta_parameterized_type`], without
    /// evaluating a spelling that has not been seen yet.
    // Cost: O(1) expected, one hash probe.
    pub(crate) fn cached_meta_parameterized_type(&self, constraint: &str) -> Option<Value> {
        self.caches
            .meta_parameterized_types
            .get(constraint)
            .cloned()
    }

    /// Evaluate every `C[T]` type a routine declaration names -- its
    /// parameters' and its return type -- so that a later `Signature` value,
    /// built with only shared access to the interpreter, reports
    /// `Parameter.type` and `.returns` as the type objects the declaration
    /// denotes. Rakudo evaluates `C[T]` once, when it compiles the signature;
    /// a trait handler (upstream NativeCall's `check_routine_sanity`) reads
    /// `.REPR` and `.^can` off those type objects.
    // Cost: O(p * n) plus each new spelling's meta-method, p = parameters,
    // n = chars of a constraint.
    pub(crate) fn resolve_decl_parameterizations(
        &mut self,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&str>,
    ) {
        let pending: Vec<String> = param_defs
            .iter()
            .filter_map(|p| p.type_constraint.as_deref())
            .chain(return_type)
            .filter(|t| t.contains('[') && !self.caches.meta_parameterized_types.contains_key(*t))
            .map(str::to_string)
            .collect();
        for t in pending {
            self.meta_parameterized_type(&t);
        }
    }

    /// The type object `C[T, ...]` evaluates to, by calling `C`'s
    /// `^parameterize` with the resolved type arguments.
    // Cost: O(a) plus the meta-method's own cost, a = type arguments.
    fn eval_meta_parameterized(&mut self, constraint: &str) -> Option<Value> {
        let (base, args) = split_parameterization(constraint)?;
        let base_ty = self.constraint_type_value(base)?;
        let ValueView::Package(base_name) = base_ty.view() else {
            return None;
        };
        if self.is_role(&base_name.resolve())
            || !self.has_user_method(base_name.as_str(), "^parameterize")
        {
            return None;
        }
        let mut how_args = vec![base_ty.clone()];
        for arg in args {
            how_args.push(match self.constraint_type_value(arg) {
                Some(v) => v,
                // A nested parameterization (`C[C[int32]]`) or a native type
                // name the registry does not list: the spelling is the type.
                None => Value::package(Symbol::intern(arg)),
            });
        }
        self.call_method_with_values(base_ty, "^parameterize", how_args)
            .ok()
    }

    /// The type object a bare type name in a constraint refers to: a term or
    /// lexical binding first (upstream NativeCall exports
    /// `my constant CArray = NativeCall::Types::CArray`), then a registered
    /// class or role.
    // Cost: O(1).
    fn constraint_type_value(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.type_name_binding(name)
            && matches!(v.view(), ValueView::Package(_))
        {
            return Some(v);
        }
        self.resolve_type_object(name)
    }
}

#[cfg(test)]
mod tests {
    use super::split_parameterization;

    #[test]
    fn splits_top_level_arguments_only() {
        assert_eq!(split_parameterization("C[Str]"), Some(("C", vec!["Str"])));
        assert_eq!(
            split_parameterization("M::C[int32, C[Str]]"),
            Some(("M::C", vec!["int32", "C[Str]"]))
        );
        assert_eq!(split_parameterization("C"), None);
        assert_eq!(split_parameterization("C[]"), None);
        assert_eq!(split_parameterization("C[Str]:D"), None);
    }
}
