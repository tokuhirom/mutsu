//! The container an `is copy` parameter binds.
//!
//! `sub f(@b is copy)` is `my @b = arg`, and `sub f(%h is copy)` is
//! `my %h = arg`: the parameter owns a fresh container, so a mutation inside
//! the routine never reaches the caller. For `@` that container is always a
//! plain, mutable, untyped `Array`, whatever the argument was: an immutable
//! `List` (`f(<x y>)`), a typed `Array[Int]`, or a native `array[uint8]`
//! (Crypt::RC4 hands `my uint8 @buf` to `RC4(@buf is copy --> Array)`).
//! Every binder (sub, method fast path, named and default arguments) goes
//! through [`Value::into_param_copy`] so they cannot disagree.

use super::*;

impl Value {
    /// The container an `is copy` parameter spelled with `sigil` binds for
    /// this argument. A non-container argument is returned unchanged; a lazy
    /// `Seq` or `gather`, which needs the interpreter to reify, is left to the
    /// caller.
    // Cost: O(1) for an unshared argument; O(n), n = elements, when the argument
    // is shared, a `List`, typed or native (the elements are copied).
    pub(crate) fn into_param_copy(self, sigil: char) -> Value {
        if !matches!(self.view(), ValueView::Array(..) | ValueView::Hash(..)) {
            return self;
        }
        let mut copy = self.copy_for_list_assignment();
        if sigil != '@' {
            return copy;
        }
        if let ValueView::Array(gc, kind) = copy.view() {
            if matches!(kind, ArrayKind::List | ArrayKind::ItemList) {
                // Keep the element data and type metadata; only the kind
                // changes, so element assignment and `.push` work.
                copy = Value::array_with_kind(Gc::new((**gc).clone()), ArrayKind::Array);
            } else if kind == ArrayKind::Array && (gc.has_native_backing() || gc.has_type_meta()) {
                copy = Value::real_array(gc.items().to_vec());
            }
        }
        copy
    }
}
