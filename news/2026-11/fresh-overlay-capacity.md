# A fresh frame overlay starts with room for its first writes

A call frame's `Env` overlay shares one process-wide empty map until its first
write. The copy-on-write of that empty map produced a map with no capacity, so
a method frame's first few writes (parameters, class-body statics, role
bindings) regrew it two or three times. The first write now allocates room
for seven entries directly. The change is small: the AttrX::Mooish t/090 probe
drops 0.48% Ir per round, and the `bench-ctor`/`method-call` rows drop 0.2-0.3%
(#18274).
