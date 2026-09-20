use AttrDeclScopeBase;
# This compunit never imports AttrDeclScopeTypes either.
unit class AttrDeclScopeChild is AttrDeclScopeBase;

method base() { AttrDeclScopeBase.new }
