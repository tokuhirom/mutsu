# Two by-name probes skip their interns

A routine's free-variable read asks whether a package-block `my` lexical of
the running package holds the name. It derived the store key first, which
interned the name and checked the frame's authoritative captures. It now asks
the package-lexical table's key union first, and an unqualified name no
package holds is answered at once.

A method call's class-body statics plan is now looked up by the owner class's
symbol, which the call already holds, instead of interning the class name on
every call (#18274).
