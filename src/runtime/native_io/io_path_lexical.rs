use super::*;

impl Interpreter {
    /// Whether `class_name` is one of the built-in `IO::Path` family classes
    /// (`IO::Path` plus the SPEC-variant subclasses) whose pure lexical methods
    /// the VM may dispatch natively through the method table's `IO::Path` rows.
    /// A user subclass that overrides a method is dispatched as compiled bytecode
    /// before the catch-all is reached, so restricting the native fast path to the
    /// built-in family keeps any user override authoritative.
    pub(crate) fn is_io_path_lexical_class(class_name: &str) -> bool {
        matches!(
            class_name,
            "IO::Path"
                | "IO::Path::Unix"
                | "IO::Path::Win32"
                | "IO::Path::Cygwin"
                | "IO::Path::QNX"
        )
    }
}
