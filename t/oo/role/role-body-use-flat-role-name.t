use lib 't/lib';
use Test;

# Regression (#8646, shape 1): a parameterized role's own deferred-body
# `use` statement (importing a custom infix operator into the role's OWN
# package) worked when the role's type argument was `::`-namespaced
# (`Bar::Comparable`), but failed the moment the role and its type argument
# were FLAT, plain names with no `::` in them at all (`FlatRoleComparable`).
#
# Method dispatch only anchors `current_package` to the receiver's own class
# when that class name is `::`-qualified (`owner_class.contains("::")`,
# among a couple of other narrow conditions) -- a role-pun whose whole name
# is flat (`FlatRoleHolder[FlatRoleComparable]`) never matched any of them,
# so `current_package` stayed whatever the CALLER happened to have (GLOBAL
# for a call from the mainline), and `bare_name_packages()`'s search list
# never included `FlatRoleHolder` -- the package the imported operator was
# actually registered under. A private method calling that operator then
# died with "Two terms in a row" on its very first call.

use FlatRoleMaker;

plan 1;

my $holder = make-flat-holder();
$holder.insert(make-flat-item(2));
$holder.insert(make-flat-item(1));
is $holder.compare-first-two(), Order::More,
    "a flat (non-namespaced) role's own method can call its use-imported operator";
