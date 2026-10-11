# A user `method message` is no longer run when an exception is thrown

`die X::Foo.new` and `X::Foo.new.throw` evaluated the class's `method message`
eagerly to fill the error text. Rakudo only runs it when something asks for the
text. The difference was visible as side effects, and as a process crash in
Compress::Bzip2: its `X::Bzip2.message` calls `fclose($!handle)` with an unset
handle, which `t/10-basic.t` never reaches under `dies-ok { decompress(...) }`
on Rakudo but did reach on mutsu, aborting with SIGSEGV inside libc.

The throw now carries the exception object and a `Died with <class>` fallback
string; `.message`, `CATCH`, `.gist` and the top-level handler compute the
text on demand. The throw-time dynamic variables stay stamped on the exception
so a lazily computed message still sees them.
