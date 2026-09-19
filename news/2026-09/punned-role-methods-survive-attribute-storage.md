Method dispatch now preserves the methods of a punned role when its instance is
stored in another object's attribute. This fixes relation lookups in modules
that pass a role-punned database object through model and row instances.
