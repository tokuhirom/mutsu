# A quote inside a regex character class is not a string delimiter

`/ <["\#]> '$BUNDLE_ROOT' /` interpolated `$BUNDLE_ROOT` as a variable: the
regex quote cursor used by the interpolation pre-pass treated the `"` in the
class as the start of a double-quoted atom, so the following single-quoted
literal looked like it sat inside one. The cursor now skips `<[...]>`,
`<-[...]>`, `<+[...]>` classes whole. App::Ariza t/12-launcher passes.
