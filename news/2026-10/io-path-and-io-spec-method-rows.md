# IO::Path and IO::Spec: 122 more built-in methods are table rows, with slurpy rows and owner lookup

Slice 3E of [ADR-11276](../../docs/adr/11276-built-in-methods-are-handler-rows.md) moved the methods
of `IO::Path` and of the four `IO::Spec` classes into the built-in method table: 943 rows are
registered now, up from 821. `IO::Path` (75 rows) is a shape that covers the class and its
`Unix`, `Win32`, `Cygwin` and `QNX` variants, and a handler hands the receiver's own class back, so
`IO::Path::Win32.new('x').parent` is still an `IO::Path::Win32`. The rows are the lexical methods
(`basename`, `parent`, `add`, `extension`, `cleanup`, ...), the cwd ones (`absolute`, `relative`,
`CWD`, `raku`), the 19 `stat` readers and file tests, the content reads (`slurp`, `lines`, `words`,
`comb`, `open`), the filesystem mutations (`spurt`, `mkdir`, `unlink`, `chmod`, `copy`, `rename`,
`move`, `symlink`, `link`) and `child`, `resolve`, `dir`, `watch` and `Numeric`. The `try_io_path_*`
name gates and the six blocks that called them from the VM's method-call chains are gone: each
primitive is one `Interpreter` method that its row calls, and the `lines($path)` and `words($path)`
sub forms call the same ones.

`IO::Spec::Unix`, `Win32`, `Cygwin` and `QNX` (47 rows) were one 560-line block of
`call_method_with_values`. One handler per method now reads which class the receiver is, and every
class Rakudo says declares the method has a row for it; `Win32`, `Cygwin` and `QNX` reach
`IO::Spec::Unix`'s rows for what they do not override, as in Rakudo. Their shapes are type-object
only, so `$*SPEC.catdir(...)` is answered from the table.

Two mechanisms came with it. `RowFlags::SLURPY` registers a `*@parts` row at every arity from its own
up (`IO::Path.add` shrinks from five rows to one, and a call with ten children is answered), and
`invoke_owner` finds a row by its owner for a receiver the table has no shape for: an instance of a
user subclass of `IO::Path`, or a call the guard step declined for a named argument it does not
declare.

An `IO::Spec` method called with fewer positionals than Rakudo's signature requires (`join('', 'a')`,
`canonpath()`) is no longer answered with a lenient guess: no row takes it, and it fails. `IO::Path`
no longer claims `starts-with` as its own native method (it is `Cool`'s), so it is answered through
the `Cool` stringification like `ends-with`. `IO::Handle`, `IO::CatHandle`, sockets, `Proc::Async`,
`Promise`, `Channel`, `Supply`, the schedulers and `Lock` are the slice's remainder; the ADR lists
each with its reason.
