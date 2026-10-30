# `self{k} = v` in a submethod no longer replaces the invocant with a stale env `self`

After an element assignment on an object (`self{$k} = $v` dispatching a user
`ASSIGN-KEY`), the op refreshed the target's local slot from the env entry of
the same name. For `self` that env entry is a leftover (ADR-14843: `self` is the
frame record's alone), and under PDF's `COSAttrHOW.compose` accessor it was the
Attribute the accessor closure captured, so the rest of `PDF::COS::Dict::TWEAK`
ran with an Attribute as `self` ("No such method 'required-entries' for
invocant of type 'Attribute+{COSDictAttrHOW}'"). The refresh now skips `self`.
PDF 0.6.17's `t/cos-tie-entry.t` matches raku (5/5), as do the other four
`cos-*` files.
