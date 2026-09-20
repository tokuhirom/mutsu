use lib 't/lib';
use Test;
# Deliberately importing only the leaves: the point is that a default expression
# resolves the type names its DECLARING compunit imported, not the ones the
# constructing scope happens to have.
use AttrDeclScopePlainChild;
use AttrDeclScopeChild;
use AttrDeclScopeBuilder;

# `has ScopeHandle $!handle` carries the bare type name as its default
# expression. mutsu evaluated that expression anchored on the class being
# CONSTRUCTED, so a subclass (or a role consumer) in another compunit resolved
# the bareword against a package that never imported the type and degraded it to
# the plain string "ScopeHandle" -- which then failed the attribute's own type
# check. Reduced from `Selkie::UI` 0.0.4, whose `Selkie::Widget` role declares
# `has NcplaneHandle $!plane` (#8842).

plan 7;

my $plain-child = AttrDeclScopePlainChild.new;

is $plain-child.base-handle.^name, 'AttrDeclScopeTypes::ScopeHandle',
   'the declaring class itself resolves its own typed-attribute default';

is $plain-child.handle.^name, 'AttrDeclScopeTypes::ScopeHandle',
   'a subclass in another compunit resolves the INHERITED default in the base scope';

my $child = AttrDeclScopeChild.new;

is $child.base.handle.^name, 'AttrDeclScopeTypes::ScopeHandle',
   'the consuming class resolves a role-composed typed-attribute default';

is $child.base.mode.^name, 'ScopeMode',
   'a role attribute defaulting to an imported enum value resolves it too';

is $child.handle.^name, 'AttrDeclScopeTypes::ScopeHandle',
   'a subclass of the consuming class inherits the role default WITH its scope';

is $child.mode.^name, 'ScopeMode',
   'the inherited enum-valued default resolves through the role too';

# The nested case: the inner `.new` runs while the builder's own package and
# compunit are current, which is what an anchor on "whatever is being
# constructed" got wrong most visibly.
is AttrDeclScopeBuilder.new.obj.handle.^name, 'AttrDeclScopeTypes::ScopeHandle',
   'a `.= new` attribute default constructs with the inner class own scope intact';
