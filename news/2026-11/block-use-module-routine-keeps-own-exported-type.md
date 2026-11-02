# A block-scoped `use`d module routine keeps its own exported type

A routine imported by a block's `use` read a bare type name through the
program's type-only lexical key when the program declared a `my class` of the
same name. With a module that exports `class Widget` and `sub make-widget() is
export { Widget.new }`, `{ use M; make-widget() }` built the program's `Widget`
instead of the module's:

```raku
my class Widget { method who { "program" } }
{ use M; say make-widget().^name }   # rakudo: M::Widget, mutsu was: Widget
```

The bareword read treated the program's key as the running frame's own
declaration whenever the frame hid the block's imported names
(`frame_hides_unit_name`, ADR-8751). That is right for the program's own
top-level routine, and wrong for a loaded module's code, which reads the
program's key only through its caller's env. The bareword ops now ask
`code_is_loaded_module_code` (the `static_name_lookup` requirement of
`code_is_foreign_unit` split out) and skip that rule for such code, so the
module's own type resolves as it does outside a block.

Pinned by `t/modules/import-export/block-use-module-routine-keeps-own-exported-type.t`
(ADR-8751 stage 1 remainder: a program class beside a block-imported type).
