# A class's export tags are a field of the declaration

`Stmt::ClassDecl` gains `export_tags: Option<Vec<String>>`. The lexical `my class ... is export`,
the lexical grammar and `unit class A::B::C is export` record their tags there; the
`__mutsu_export_type` entry of `custom_traits` is gone, with its builder, its constant, the RakuAST
`lexical_export_tags` reader and the five scans of it (module declaration scan, nested-export scan,
role-body lexical registration, exported-type-name scan, the `RegisterClass` publication).
