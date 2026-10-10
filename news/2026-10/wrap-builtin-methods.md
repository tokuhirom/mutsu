# `.wrap` on built-in value methods

`Str.^find_method('uc').wrap(...)` and `DateTime.^find_method('year').wrap(...)`
now take effect: the VM method-call entry consults the built-in wrap chain
(O(1) when nothing is wrapped), alongside the existing `IO::Handle` support.
