# Preserve itemized Lists in the `X` routine form

The routine form `infix:<X>(...)` now keeps an itemized List as one element of each cross-product tuple. This also corrects `XX`, which uses `X` internally, while ordinary Array operands continue to expand into their elements.
