# App::Lorea's watch-and-rerun chain: four interpreter gaps behind one file watcher

Checking #9586's new `IO::Notification.watch-path` against App::Lorea, the
tool it was filed for, showed the watcher itself matched rakudo except in two
details. The rest of the chain from "a file changed" to "the command ran" broke
on four general mutsu bugs, all fixed here.

**Watcher parity.** A file created and written between two polls is now
reported the way libuv reports it: `FileRenamed` for the new entry, then
`FileChanged` for the write. The watcher tells this apart from a file moved in
by `rename` (which is `FileRenamed` alone) by whether the inode's last change
was a content write (ctime == mtime). An `IO::Path` argument to `watch-path` is
now reported under its absolute path, as rakudo does.

**Sigilless method-literal invocants.** `anon method (\SELF: |) { … }` is the
wrapper idiom of OO::Monitors (and so of every `monitor`, Timer::Stopwatch
included). The parser bound `SELF` without the sigilless marker, so the body's
bare `SELF` compiled to a bare-word lookup. That lookup only found the binding
when the env happened to be synced, which loading any module did. So a program
with no `use` saw the invocant as `(Any)`, the monitor's lock was never set,
and every monitor method died with "No such method 'lock' for invocant of type
'Lock'". The test suites never noticed because `use Test` is itself a module
load.

**Private methods under a custom `add_method`.** mutsu keys `method !stop`
under the same name as `method stop`. A HOW that re-adds every public method
through the native `Metamodel::ClassHOW::add_method` (OO::Monitors) therefore
replaced the name's whole candidate list, and `self!stop` stopped resolving.
The native `add_method` now keeps the name's private candidates, and the HOW
protocol hands `add_method` only the public ones.

**`whenever` on an object with a `Supply` method.** `whenever $source` now
coerces any object that declares its own `Supply` method, as rakudo's
`$source.Supply` does. Only `Supplier` and `Proc::Async` were special-cased
before, so `whenever $stopwatch` treated the Timer::Stopwatch object as a
single value and never subscribed to its ticks.

**`.new` on a built-in instance.** `$proc .= new(@args)`, `$lock.new`,
`$promise.new` and `$path.new('b')` now construct a new object of the
receiver's type. Before, they answered `Nil`, "No native method 'new'" or
"Unknown method value dispatch", because the built-in constructors were keyed
on the type object.

Two residues keep Lorea's own `t/file-change.t` from matching rakudo, and are
filed separately. One is #9609: a `SetHash` in an attribute is not mutated by
`.grab`/`.set`/`.unset`, so Lorea's backlog queue never empties. The other is
#9610: an escaping regex's `<$_>` reads the match subject instead of the
literal's captured topic, which breaks Lorea's `--regex` filters.
