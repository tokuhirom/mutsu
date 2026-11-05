# `%?RESOURCES<libraries/foo>` answers `.platform-library-name`

NativeCall's `guess_library_name(Distribution::Resource $lib)` calls
`$lib.platform-library-name`, which a resource did not have, so a module declaring
`is native(%?RESOURCES<libraries/foo>)` died at its first native call. A resource
now answers it with the IO::Path of its (already platform-resolved) library file;
a plain IO::Path still has no such method, as in rakudo. Sort::Fast's tests 1 and 2
reach the native sort.
