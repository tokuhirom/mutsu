# `import Foo` of a self-declared package that exports nothing

`module TestModule { our sub f() {...} }; import TestModule; TestModule::f` died with "No exports
found for module: TestModule"; rakudo treats the import as a no-op. The `import` statement now
tolerates an export-less package the unit declared itself, while an unknown name still fails. Found
by `L10N::ZH`'s `t/11-use-import.rakutest` (its `导入` statement), which now matches rakudo.
