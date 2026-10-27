# A parameter records the form of its target

`ParamDef` now carries `target: ParamTarget` (`Variable`, `TypeOnly`, `TypeCapture`, `Parens`,
`Brackets`), set where the parser builds a bare-type, bare `::T` or anonymous `(…)` / `[…]`
parameter. The RakuAST conversion and lowering read it instead of recognizing the execution
placeholders (`__type_only__`, `__subsig__`, `@`, `__for_unpack*`, `__type_capture__T`) by name.
`.AST` of every signature form is unchanged. ADR-10723 section 6.113.
