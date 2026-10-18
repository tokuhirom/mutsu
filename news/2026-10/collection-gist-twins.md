# `gist` of a collection with user-rendered elements is the collections' twin

The by-name `gist` gate that rendered a collection holding an instance, a type object, a code
object or a mixin through method dispatch is gone (ADR-11276 §9.188): the renderer is the late
interpreter twin of `List`/`Map`/`Seq`/`Pair`'s `gist`, reached once the pure rows decline, and the
pure probe now declines a mixin element as well. Output is unchanged.
