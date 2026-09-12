# NativeCall exposes its TEST library helper

mutsu now supports NativeCall's :TEST export tag and provides the
guess_library_name helper with its string, versioned, list, callable, and
path forms. Unknown NativeCall export tags continue to raise
X::Import::NoSuchTag.
