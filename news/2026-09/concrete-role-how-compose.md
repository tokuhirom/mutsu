# Compose concrete roles created through the MOP

Types minted by `Metamodel::ConcreteRoleHOW.new_type` now have a role definition
and dispatch metaobject methods such as `.^compose` and `.^roles` through their
concrete role HOW.
