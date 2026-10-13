Grouped attribute declarations retain one RakuAST signature shape while
preserving each member's runtime traits and defaults through EVAL. Attribute
colon argument lists retain nested declarations during the round trip, even
though the visible RakuAST declaration matches Rakudo's source rendering.
