# `.wrap` on a built-in method takes effect

Terminal::MultiProgress's test helper captures output the way many Raku
test suites do: it wraps `IO::Handle.print` and collects what is printed.

```raku
my $print = $*OUT.^find_method('print');
my $wrapped = $print.wrap: method (|c) { $text ~= c.list.join }
```

mutsu registered the wrap and returned a `Routine::WrapHandle`, but nothing
ever ran it. A wrap is consulted where a user method's `MethodDef` is
dispatched, and a built-in method has none. So every frame of the progress
display went to the terminal and the tests saw an empty capture.

The native dispatch doors now consult the wrap chain of a built-in class's
method. These are the shared native instance-method entry, the `IO::Handle`
output fast path, and the `print` routine's write to `$*OUT`. The chain runs
as a dispatcher wrap already does: the innermost `callsame` re-dispatches the
method with the chain bypassed, which reaches the native implementation. This
also works for a `print` inside a `start` block.
