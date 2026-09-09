# `ContainerRef` can carry a holder-local itemization flavour

The NaN-box representation now distinguishes a plain `ContainerRef` from an
itemized holder without changing the shared cell or the public `ValueView`:
both flavours project as `ValueView::ContainerRef`, and existing constructors
continue to create the plain flavour.

`Value::container_ref_itemized(cell)` and
`Value::container_ref_is_itemized()` provide the representation seam needed by
the later holder-semantics slice of [ADR-0079](../../docs/adr/0079-container-itemization-is-a-holder-property-tagged-on-the-containerref-word.md).
The new `Kind::ContainerRefItemized` is included in encoding, decoding, tag
probes, variant comparisons, stringifiable-list checks, and GC tracing, with
round-trip tests pinning the cell identity and flavour bit.

No runtime producer opts into the new flavour yet, so this slice is
behaviour-neutral.
