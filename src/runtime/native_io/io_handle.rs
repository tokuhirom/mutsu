use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// Mutable dispatch for `IO::Handle` methods that mutate the receiver in
    /// place. Currently only `.open`: in Raku `$fh.open(...)` opens the handle
    /// *and returns self*, so a later `$fh.print`/`$fh.print-nl` operates on the
    /// now-opened handle. mutsu's `.open` builds a fresh opened-handle instance
    /// (the handle id lives in the handle table); to match Raku, the receiver's
    /// attributes must be replaced with the opened handle's so the caller's
    /// binding (`$fh`, or the `with` topic `$_`) reflects the open. The returned
    /// `updated` map is written back to the receiver by the caller
    /// (`write_back_sharing`). Any other method falls back to the immutable path
    /// via the sentinel "No native mutable method" error.
    pub(in crate::runtime) fn native_io_handle_mut(
        &mut self,
        attributes: AttrMap,
        method: &str,
        args: Vec<Value>,
        _publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        if method == "open" {
            let result = self.native_io_handle(&attributes, "open", args)?;
            // A successful open returns an `IO::Handle` instance carrying the new
            // handle id; an error returns a `Failure`. Only mutate the receiver on
            // success — on failure the handle stays unopened (as in Raku).
            if let ValueView::Instance {
                class_name,
                attributes: new_attrs,
                ..
            } = result.view()
                && class_name == "IO::Handle"
            {
                let updated = new_attrs.as_map().clone();
                return Ok((result, updated));
            }
            return Ok((result, attributes));
        }
        Err(RuntimeError::new(format!(
            "No native mutable method '{}' on 'IO::Handle'",
            method
        )))
    }

    /// The attributes an `IO::Handle` row reads of its receiver: the handle id
    /// and the options the handle was made with.
    // Cost: O(a), a = attributes of the instance (one copy of the map).
    pub(crate) fn io_handle_attrs(target: &Value) -> AttrMap {
        match target.view() {
            ValueView::Instance { attributes, .. } => AttrMap::clone(&attributes.as_map()),
            _ => AttrMap::new(),
        }
    }

    /// The native methods of an `IO::Handle` instance whose class has no shape
    /// in the method table (a user subclass, `IO::Pipe`, a socket, reached by
    /// its MRO) and the calls the guard step declined. Every method
    /// `IO::Handle` declares is a row of the table and is answered through its
    /// owner (ADR-11276 §9.20).
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(crate) fn native_io_handle(
        &mut self,
        target: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // `Mu.perl` is `self.raku`, and `slurp-rest` is the deprecated spelling
        // of `slurp`; the rows of `raku` and `slurp` answer them.
        let row_method = if method == "perl" { "raku" } else { method };
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            &["IO::Handle", "Mu"],
            row_method,
            &args,
            || Value::make_instance_without_destroy(Symbol::intern("IO::Handle"), target.clone()),
        ) {
            return result;
        }
        Err(RuntimeError::new(format!(
            "No native method '{}' on IO::Handle",
            method
        )))
    }
}
