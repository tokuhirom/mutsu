# `$obj[i].attr = v` through a `handles <AT-POS>` delegation replaces `$obj` with the delegate

Assigning to an attribute of an element reached through a delegated `AT-POS`
overwrites the *object* variable with the delegate container, so every later
method call on it dies:

```raku
class Elem { has $.a is rw; }
class Box does Positional {
    has @!c handles <AT-POS elems>;
    submethod BUILD() { @!c := Array[Elem].new(:shape(2)); @!c[0] = Elem.new; @!c[1] = Elem.new; }
    method tag() { "box" }
}
my $b = Box.new;
$b[0].a = 11;
say $b[0].a;    # 11        — the assignment itself works
say $b.tag;     # raku: box   mutsu: No such method 'tag' for invocant of type 'Array'
```

`$b` is an `Array` afterwards. The read (`$b[0].a`) still works because it goes
through the array; only a method call reveals that the variable's binding was
replaced.

Pre-existing (reproduced on `main` at 4acbdcd09, unrelated to the parameterised
role-pun work). The two-step lvalue `$obj[i].attr = v` resolves `$obj[i]` to the
element and then publishes the mutated container back over the *outer* variable
name instead of into the object's attribute — most likely
`overwrite_array_bindings_by_identity` reaching a binding it should not, or the
subscript-lvalue path recording the wrong target variable.

## Why it matters

It is the last blocker for `NativeHelpers::CStruct`'s `LinearArray`, whose whole
shape is `$arr[$i].field = $v` over a delegated cache:

```raku
my $arr = LinearArray[MYSQL_BIND].new(3);
$arr[0].buffer = $ptr;      # dies with X::Assignment::RO on the *second* element assign
$arr.dispose;               # 'No such method dispose for invocant of type Array'
```

With the pun/`BUILD`/delegation-write-back fixes landed, `LinearArray` now
allocates, computes its stride, indexes, and nativecasts correctly — this is all
that remains before the mysql driver's `BPointer`/`pointer-to`/`BODY_OF` surface
closes. Minimal repro above needs no database and no native library.
