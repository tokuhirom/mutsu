use super::*;

/// The `IO::Handle.open` options that mean something for a handle already
/// attached to a live stream; every other option selects a file open mode,
/// which such a handle has no use for.
const IN_PLACE_OPEN_OPTIONS: [&str; 6] = ["chomp", "nl-in", "nl-out", "out-buffer", "bin", "enc"];

impl Interpreter {
    /// Re-open a handle that is already attached to a live stream (a standard
    /// handle, `$*ARGFILES`, a socket): there is nothing to open, so only the
    /// per-handle options the caller passed explicitly are applied, in place.
    ///
    /// Only *explicit* arguments are honoured: Rakudo defaults each of these
    /// options to the handle's current value, so `$*OUT.open(:w)` must leave an
    /// earlier `$*OUT.nl-out = "|"` alone rather than resetting it to `"\n"`.
    fn apply_open_options_in_place(
        &mut self,
        handle: &Value,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        let explicit: std::collections::HashSet<String> = args
            .iter()
            .filter_map(|arg| match arg.view() {
                ValueView::Pair(name, _) => Some(name.clone()),
                _ => None,
            })
            .collect();
        if !IN_PLACE_OPEN_OPTIONS
            .iter()
            .any(|key| explicit.contains(*key))
        {
            return Ok(());
        }
        let (.., bin, line_chomp, line_separators, out_buffer_capacity, nl_out, enc, _, _) =
            self.parse_io_flags_values(args);
        self.with_handle_mut(handle, |state| {
            if explicit.contains("chomp") {
                state.line_chomp = line_chomp;
            }
            if explicit.contains("nl-in") {
                state.line_separators = line_separators;
            }
            if explicit.contains("nl-out") {
                state.nl_out = nl_out.unwrap_or_else(|| "\n".to_string());
            }
            if explicit.contains("out-buffer") {
                state.out_buffer_capacity = out_buffer_capacity;
            }
            if explicit.contains("bin") {
                state.bin = bin;
                state.encoding = if bin {
                    "bin".to_string()
                } else {
                    "utf-8".to_string()
                };
            }
            if explicit.contains("enc")
                && let Some(enc) = enc
            {
                state.bin = false;
                state.encoding = enc;
            }
            Ok(())
        })?;
        Ok(())
    }

    // Cost: TODO
    pub(crate) fn io_handle_open(
        &mut self,
        target_val: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let target = Self::io_handle_attrs(target_val);
        // A handle already bound to a standard stream has no filesystem
        // path to reopen: its `path` attribute is the sentinel name
        // ("STDOUT"/"STDERR"/"STDIN"), which `.path` reports as
        // `IO::Special.new("<STDOUT>")`. Resolving that name as a file
        // would create a real file called `STDOUT` in the CWD *and*,
        // because `.open` writes the opened handle back over the
        // receiver, silently redirect the process's own output into it.
        // Rakudo instead re-applies the given per-handle options to the
        // live stream and returns `self`, so `$*OUT.open(:w) === $*OUT`.
        // (The `open` *sub* on `$*OUT.path` keeps its own fresh-handle
        // behaviour, which matches Rakudo too; see `builtin_open`.)
        if let Some(id) = Self::handle_id_from_value(target_val)
            && self
                .io_handles()
                .map
                .get(&id)
                .is_some_and(|state| state.target != IoHandleTarget::File)
        {
            self.apply_open_options_in_place(target_val, args)?;
            return Ok(target_val.clone());
        }

        // IO::Handle.new(:path(...)).open(:w, :nl-out(...))
        // Merge instance attributes with open args (args override instance attrs)
        let path_str = target
            .get("path")
            .map(|v| match v.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IO::Path" => attributes
                    .as_map()
                    .get("path")
                    .map(|p| p.to_string_value())
                    .unwrap_or_default(),
                _ => v.to_string_value(),
            })
            .unwrap_or_default();
        let path_buf = self.resolve_path(&path_str);

        // Build merged args: instance attributes as defaults, open args override
        let mut merged_args = Vec::new();
        // Collect which keys the open() args explicitly specify
        let mut explicit_keys: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for arg in args {
            if let ValueView::Pair(name, _) = arg.view() {
                explicit_keys.insert(name.clone());
            }
        }
        // Add instance attributes as pairs (only if not overridden by open args)
        for (key, value) in target.iter() {
            let key = key.as_str();
            if key == "handle" || key == "path" || key == "mode" {
                continue;
            }
            if !explicit_keys.contains(key) {
                merged_args.push(Value::pair(key.to_string(), value.clone()));
            }
        }
        merged_args.extend(args.iter().cloned());

        let (
            read,
            write,
            append,
            bin,
            line_chomp,
            line_separators,
            out_buffer_capacity,
            nl_out,
            enc,
            create,
            exclusive,
        ) = self.parse_io_flags_values(&merged_args);
        match self.open_file_handle(
            &path_buf,
            read,
            write,
            append,
            bin,
            line_chomp,
            line_separators,
            out_buffer_capacity,
            nl_out,
            enc,
            create,
            exclusive,
            Some(std::path::Path::new(&path_str)),
        ) {
            Ok(handle) => Ok(handle),
            // Like the `open` sub, `IO::Handle.open` returns a Failure
            // (wrapping the exception) on error rather than throwing.
            Err(err) => Ok(super::fs_errors::open_error_failure(err)),
        }
    }

}
