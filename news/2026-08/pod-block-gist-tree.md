# Pod blocks render as an indented gist tree

`Pod::Block.gist` now renders the same useful structural tree as Rakudo instead
of the generic instance representation. The renderer walks nested Pod blocks,
indents children by two spaces, and includes the non-empty public Pod metadata
used by the format (`config`, `name`, `level`, `caption`, `type`, and `term`).

The existing `Pod::Block::Declarator` text rendering remains unchanged. Table
rows retain their Raku representation while plain paragraph content is rendered
as indented text.

`t/pod-block-gist.t` pins the nested document tree, `$=pod` bracket rendering,
and omission of empty attributes.
