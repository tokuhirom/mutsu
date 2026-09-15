# Compound assignments now curry `*` right operands

`$value OP= *` now produces a `WhateverCode`, matching Raku. This makes callback
idioms such as `$out ~= *` defer the compound assignment until invocation instead
of eagerly stringifying `Whatever`.
