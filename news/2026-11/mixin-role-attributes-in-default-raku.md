# Default mixin rendering includes role attributes

The default `.raku` and `.gist` of a class instance with a role mixed in showed the class attributes but omitted the role's attributes. Those attributes live in the Mixin's own cell, separate from the wrapped instance's cell.

The instance renderer now reads both cells. It renders the most recently mixed role first, then earlier roles, then the class, retaining each owner's declaration order and live values. User-defined representation methods still take precedence.
