# A CStruct field's type follows a `constant` alias

A C binding names its platform-dependent types once and then writes the alias
everywhere. `DBDish::mysql::Native` is typical:

```raku
constant my_bool = int8;
constant intptr is export = ptrsize == 8 ?? uint64 !! uint32;

class MYSQL_BIND is repr('CStruct') is export {
    has intptr  $.length is rw;
    has intptr  $.is_null is rw;
    ...
    has my_bool $.is_unsigned;
}
```

Neither `intptr` nor `my_bool` is a NativeCall type name, so `FieldType::from_type_name`
returned `None` for those fields — and because one unmappable field aborts the
whole layout (continuing would give every later field a wrong offset, which is a
silent wild read), `MYSQL_BIND` had **no layout at all**. `nativesizeof(MYSQL_BIND)`
failed with "expected type with CPointer, CStruct, CArray, P6int or P6num
representation, but got a P6opaque", even though `MYSQL_BIND.REPR` correctly
answered `CStruct`.

That disagreement was the tell. It also explains a failure a long way from the
declaration: `NativeHelpers::CStruct`'s `LinearArray[::T]` computes its stride
with `my int $sol = nativesizeof(T)` in the role body, so
`LinearArray[MYSQL_BIND]` died during parameterisation and came back as the bare,
unparameterised role — whose `.elems`/`AT-POS` then did not exist.

Native *signatures* already followed these aliases (they had the same problem
with `--> my_bool` return types). `cstruct_layout` now calls the same resolver
for a field type, and only for a name that is not already marshallable, so a
field typed with a real C type or with a class held by reference keeps its
declared spelling.

Pinned by `t/cstruct-field-constant-type-alias.t`.
