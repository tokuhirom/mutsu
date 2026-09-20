use AttrDeclScopeChild;
unit class AttrDeclScopeBuilder;
# A nested construction: the inner `.new` runs while this class's own package
# and compunit are the active scope.
has AttrDeclScopeChild $.obj .= new;
