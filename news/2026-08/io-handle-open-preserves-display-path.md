# `IO::Handle.open` preserves the path as given

Opening a relative path still resolves it against the current directory for
filesystem access, but the resulting handle now retains the caller's original
path for `.Str` and `.path`.
