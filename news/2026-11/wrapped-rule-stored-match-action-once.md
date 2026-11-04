# A wrapped rule whose wrapper stores the `callsame` Match runs its action once

When a `.wrap`ped grammar rule's wrapper kept its `callsame` Match in a container,
the post-parse action walk reached that Match through the wrapper's capture node, not the
node the reduction had already dispatched, and ran the rule's action a second time.
Adopting the wrapper's Match for the filed node now also shares their "action ran"
mark (#16410).
