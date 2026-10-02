# `&!attr` reads the invocant's attribute, not the caller's same-named `$!attr`

`&!attr()` compiles to `CallOnCodeVar` on the name `!attr`, and a bare
`&!attr` compiles to `GetCodeVar` on the same name. Both consulted the env
before `self`. The env can still hold the calling method's `$!attr` under
that same sigil-less `!attr` key, so a method reached from another object's
method called the caller's attribute value. That died with "No such method
'CALL-ME' for invocant of type 'Int'".

CSS::Module's `method index { &!index() }` was the case found in the wild. It
is reached from CSS::Properties, which has its own `$!index`, and the failure
stopped CSS::TagSet's `t/tag-set-xhtml.t` (#10662). Both opcodes now resolve
a `!`-prefixed name against `self`'s live attribute cells first. The xhtml
suite now gets further and stops at a separate assignment through an `is rw`
multi method, filed as #10811.
