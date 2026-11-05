# A subscript store remembers that no user postcircumfix candidate delegated

#16310 runs the user `postcircumfix:<[ ]>` candidate in scope before every subscript store on an
object, to learn whether it hands the CORE routine another index (Array::Rounded). NativeCall
exports `postcircumfix:<[ ]>(CArray:D \array, $pos)`, which accepts every CArray store and only
reads `array.AT-POS($pos)`, so every `$c[$_] = v` resolved the candidate set and ran that body:
83k -> ~1,100k Ir per store, 13x (#16319), and Sort::Fast's million-element fill with it.

The answer "no candidate delegated" is now remembered per (lexical dispatcher in scope, target
class or composition node, index type key) and the next store of the same kinds skips the
resolution and the run. A delegating candidate (Array::Rounded) is never remembered. Also,
`has_class_scoped_subs`, asked on every method call, is memoized per class name until the
`class_subs` table is written (it scanned every class that declares a sub).

Callgrind slope on a profiling build, second run: CArray store 1,100k -> 83.0k Ir; fill +
`.Array` 186.0k Ir per element. Refs #16215. Closes #16319.
