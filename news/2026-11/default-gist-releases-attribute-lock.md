# `say $obj` no longer hangs when rendering an attribute writes the object

An instance's default `.gist` and `.raku` render each public attribute's
value. A Proxy-bound attribute runs its FETCH to produce that value, and the
FETCH may write another attribute of the same object. `AttrX::Mooish`'s lazy
builder does exactly that, bumping a `++⚛$!builds` counter. The renderer still
held the object's attribute lock while it ran the FETCH, so that write waited
for the lock forever. `say` on such an object hung.

The renderer now reads the attribute values under the lock, releases it, and
only then renders them. `t/oo/attribute/default-gist-proxy-attribute-fetch.t`
covers a Proxy FETCH that writes its own object, the same rendered from another
thread, and a lazily built value.
