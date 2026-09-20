use AttrDeclScopePlainBase;
# This compunit never imports AttrDeclScopeTypes, yet constructing it evaluates
# the base class's typed-attribute default, which names a type from there.
unit class AttrDeclScopePlainChild is AttrDeclScopePlainBase;

method base-handle() { AttrDeclScopePlainBase.new.handle }
