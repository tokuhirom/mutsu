unit module AttrDeclScopeTypes;

# A typed attribute's declaration carries the bare type name as its default
# expression, so this class's short name has to be resolvable wherever that
# default is evaluated.
class ScopeHandle is export { }

enum ScopeMode is export <ScopeOpaque ScopeBlend>;
