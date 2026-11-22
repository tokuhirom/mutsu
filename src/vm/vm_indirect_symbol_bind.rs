//! Binding through a name computed at run time (`Expr::IndirectSymbolBind`):
//!
//! ```raku
//! my $our = 'OUR';
//! $::($our)::A43::x := 44;                  # `$x` of package A43
//! ::($our)::A44 := class { our $x = 41 };  # the symbol A44 itself
//! ```
//!
//! The target is the name's last segment, bound in the stash of the package
//! the leading segments name: the same store `OUR::<$x> := v` and
//! `Foo::<&f> := &g` write, so every later spelling of the symbol
//! (`$OUR::A43::x`, `::($our)::A43.WHO<$x>`, `$::($our)::A44::x`) reads it.
//! A name whose package is lexical or dynamic (`MY`, `CALLER`, `DYNAMIC`,
//! ...), or that has no package at all, raises the same "not yet
//! implemented" rakudo raises for every run-time bind.
use super::*;
use crate::value::ValueMap;

/// What `exec_indirect_symbol_bind_op` binds: the package whose stash gets
/// the symbol, the key's sigil ("" for a package symbol) and the bare
/// symbol name.
struct BindTarget {
    package: String,
    sigil: &'static str,
    bare: String,
}

impl Interpreter {
    // Cost: O(n), n = chars of the name (split once and interned), plus the
    // hashed `our`/env stores of the bind; an `&` key also pays the
    // routine-alias registration (see `index_assign_named_package_stash`).
    pub(super) fn exec_indirect_symbol_bind_op(
        &mut self,
        code: &CompiledCode,
        sigil_idx: u32,
    ) -> Result<(), RuntimeError> {
        let sigil = Self::const_str(code, sigil_idx).to_string();
        let name = self.stack.pop().unwrap_or(Value::NIL).to_string_value();
        let raw = self.stack.pop().unwrap_or(Value::NIL);
        let (value, source) = Self::unwrap_bind_index_value(raw);
        let Some(target) = self.indirect_bind_target(&sigil, &name) else {
            return Err(RuntimeError::new(
                "This case of binding is not yet implemented",
            ));
        };
        if target.sigil == "&" {
            // A routine: aliased in the routine registry and published as
            // the package's symbol, as `Foo::<&f> := &g` does.
            self.register_package_code_alias(target.package.clone(), &target.bare, &value);
            self.publish_package_stash_symbol(target.package, "&", &target.bare, &value);
            self.stack.push(value);
            return Ok(());
        }
        // Any other key goes through the package-stash bind `Foo::{$k} := $v`
        // uses, which shares the source variable's container.
        let stash = Self::make_stash_instance(&target.package, ValueMap::default());
        let key = format!("{}{}", target.sigil, target.bare);
        let bound = self.bind_stash_key(code, &stash, &key, value, source.as_deref())?;
        self.stack.push(bound);
        Ok(())
    }

    /// Split `name` (with the op's `sigil`) into the package stash and the
    /// symbol a bind writes, or `None` for a target no package stash holds.
    // Cost: O(d), d = segments of the name (memoized split).
    fn indirect_bind_target(&self, sigil: &str, name: &str) -> Option<BindTarget> {
        let segments = crate::qualified::segments(Symbol::intern(name));
        let (last, leading) = segments.split_last()?;
        let last = last.as_str();
        // `::($p)::('$x')`: the type form names the sigil in its last segment.
        let (sigil, bare) = if sigil.is_empty() {
            match last.as_bytes().first() {
                Some(b'$' | b'@' | b'%' | b'&') => (&last[..1], &last[1..]),
                _ => ("", last),
            }
        } else {
            (sigil, last)
        };
        // A twigil (`$*x`, `$?x`, `$!x`) names a dynamic, compile-time or
        // attribute variable, which lives in no package stash.
        if bare.is_empty() || bare.starts_with(['*', '?', '!', '.', '^', '=', '~']) {
            return None;
        }
        let sigil = match sigil {
            "$" => "$",
            "" => "",
            "@" => "@",
            "%" => "%",
            "&" => "&",
            _ => return None,
        };
        let (base, path) = match leading.split_first() {
            Some((head, rest)) if head.as_str() == "OUR" => (self.current_package(), rest),
            Some((head, rest)) if head.as_str() == "GLOBAL" => (String::new(), rest),
            // Lexical (`MY`, `OUTER`, `CALLER`, ...), dynamic and setting
            // pseudo-packages are not package stashes.
            Some((head, _)) if Self::is_pseudo_package_name(head.as_str()) => return None,
            Some(_) => (String::new(), leading),
            // An unqualified name: rakudo binds the lexical, which a run-time
            // name cannot reach.
            None => return None,
        };
        let mut package: Option<Symbol> = (!crate::qualified::is_global_name(base.as_str())
            && !base.is_empty())
        .then(|| Symbol::intern(&base));
        for part in path {
            if part.as_str().is_empty() {
                return None;
            }
            package = Some(match package {
                Some(pkg) => crate::qualified::qualified(pkg, *part),
                None => *part,
            });
        }
        let package = package.map_or_else(|| "GLOBAL".to_string(), |pkg| pkg.as_str().to_string());
        Some(BindTarget {
            package,
            sigil,
            bare: bare.to_string(),
        })
    }
}
