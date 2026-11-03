//! Pair construction and named argument container transport.

use super::*;

impl Interpreter {
    /// `OpCode::MakePair`: build a data-minted Pair (ADR-0021 I2), always the
    /// positional flavour — this is every fat-arrow compile except
    /// argument-position named-arg synthesis (`OpCode::MakeNamedArg`, below).
    // Cost: O(k + l), k = key work, l = by-name locals fallback, plus Proxy/Seq evaluation.
    pub(super) fn exec_make_pair_op(
        &mut self,
        code: &crate::opcode::CompiledCode,
    ) -> Result<(), RuntimeError> {
        let (left, right) = self.pop_pair_operands_capturing(code, false);
        // The key is a decontainerized `Mu $key` of `infix:<=>`, so a Proxy key
        // is FETCHed once here (`($proxy => 1).key`, `%h = $proxy => 1`).
        let left = self.pair_key_fetching_proxy(left)?;
        // A `gather` Seq key is read when the Pair lands in a hash or a string
        // (`%h = (gather { take "k" }) => 1` keys "k"); that read cannot pull
        // the deferred source itself, so it is pulled here, once.
        if let ValueView::Seq(body) = left.view() {
            let body = body.clone();
            self.reify_seq_body(&body)?;
        }
        self.stack.push(Value::value_pair(left, right));
        Ok(())
    }

    /// `OpCode::MakeNamedArg`: build a named-argument-flavour Pair (ADR-0021
    /// I2/I3). Emitted only where the call-site syntax genuinely intends a
    /// named argument (a literal `key => value` / `:key(value)` written
    /// directly in an argument list, or a Capture literal's named-lane
    /// element `\(:$a)`) — preserving the in-band named marker those sites
    /// need to reach the callee's binder.
    // Cost: O(k + l), k = string key bytes, l = by-name locals fallback. Rakudo: O(1) -- see #9252.
    pub(super) fn exec_make_named_arg_op(
        &mut self,
        code: &crate::opcode::CompiledCode,
        readonly: bool,
    ) {
        let (left, right) = self.pop_pair_operands_capturing(code, true);
        let right = if readonly {
            Value::container_ref(crate::gc::Gc::new(
                crate::value::ContainerCell::new_readonly_binding(
                    right.deref_container(),
                    crate::ast::ReadonlyKind::Alias,
                ),
            ))
        } else {
            right
        };
        // Preserve the original key type for `.key` to return the correct type.
        // Use ValuePair for non-string keys, Pair (the named flavour) for
        // string keys — the shape every named-arg key actually has.
        match left.view() {
            ValueView::Str(_) => {
                let key = left.to_string_value();
                self.stack.push(Value::pair(key, right));
            }
            _ => {
                self.stack.push(Value::value_pair(left, right));
            }
        }
    }

    /// Shared operand pop for `MakePair`/`MakeNamedArg`: `key => $var` tags
    /// the RHS scalar variable with `WrapVarRef`, so capture its container.
    /// The Pair's value becomes a `ContainerRef` shared with `$var`, giving
    /// Raku's write-through semantics: `$pair.value = X` updates `$var`, and
    /// `$pair.value<k> = v` writes through to `$var`'s backing Array/Hash.
    /// (See S02:1704 / roast S02-types/pair.t.)
    // Cost: O(1) with a source slot; O(l) by-name fallback, l = frame locals.
    fn pop_pair_operands_capturing(
        &mut self,
        code: &crate::opcode::CompiledCode,
        named: bool,
    ) -> (Value, Value) {
        let mut right = self.stack.pop().unwrap();
        // Rakudo's `infix:<< => >>` binds the key as a plain `Mu $key` and the
        // value as `Mu alue`, so only the VALUE keeps a container: with
        // `my $s = $(1, 2)`, `($s => "x").key.raku` is `(1, 2)` and
        // `.key.VAR.^name` is `List`, not `Scalar`. mutsu handed the key
        // through whole, so an itemized key stayed itemized (`$(1, 2) => "x"`)
        // -- and a `List:D`-keyed object hash then saw a `Scalar`, not a
        // `List`. The value side is unchanged: `("k" => $t).value.raku` is
        // still `$(8, 9)` for an itemized `$t`, and the write-through capture
        // below still applies to it.
        let left = self.stack.pop().unwrap().deitemize_element();
        if let ValueView::VarRef {
            name: source_name,
            value: inner,
            ..
        } = right.view()
        {
            let source_name = source_name.resolve();
            let inner = inner.clone();
            let slot_hint = right.varref_slot();
            // `box_type_objects` is set: an UNINITIALIZED declared scalar
            // (`my Int $x`, which holds the bare type object `Int`) is still a
            // container, and raku aliases it — `my Int $x; my $p = ("k" => $x);
            // $p.value = 5` leaves `$x` at 5. Without boxing, the Pair captured
            // the bare type object, `$p.value = 5` had nothing to write
            // through to, and the write was faked by rebinding `$p`'s own env
            // entry: the Pair printed `k => 5` while `$x` stayed `Int`.
            right = if named {
                let cell = self.capture_rw_arg_cell(code, &source_name, inner, slot_hint);
                self.register_container_cell_constraint_for_name(&cell, &source_name);
                cell
            } else {
                self.capture_var_cell_boxing_type_objects(code, &source_name, inner, slot_hint)
            };
        }
        (left, right)
    }
}
