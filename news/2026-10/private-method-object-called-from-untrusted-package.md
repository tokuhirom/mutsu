# A private method object is callable as a routine from a package the owner does not trust

Rakudo checks a private method's trust at the `!` call site only. A private method *object*
(`$type.^find_private_method('m')`) called as a routine (`$type.&handle()`) is not checked, so code
in any package may call it. mutsu pushed the owner as the caller class for such a call, but
`private_calling_package` preferred the routine frame of the real caller (a role method, a closure)
over the pushed class, and the call died with `Cannot call private method ... does not trust ...`.

The push now marks its `method_class_stack` frame as authorizing private calls, and
`private_calling_package` answers that frame first. A fully qualified `$o!Owner::m` is still checked
at its call site, unchanged.

AttrX::Mooish's `invoke-composer` calls a private composer this way (#13264): `t/060-composer`
now passes on the real module. Pinned in `t/modules/private-method-handle-call-from-other-package.t`
(passes under `raku` too).
