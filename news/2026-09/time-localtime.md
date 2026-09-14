# Time::localtime works with mutsu

mutsu now runs the `Time::localtime` distribution. Its exported `localtime` and
`ctime` routines, `:FIELDS` variables, NativeCall symbol traits, and the
`Time::localtime` return object work through the normal parser, compiler, and VM
pipeline.
