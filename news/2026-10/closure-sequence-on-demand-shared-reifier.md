# Closure sequences run on demand, and a lazy array's clone shares the reifier

`my @a = 1, { ... } … *` no longer pre-computes 32 elements while the right-hand side is still
being evaluated: the generator runs when elements are asked for. That lets a generator read the
array it is assigned to (`our @G = 0, {++$ - @G[@G[$++]]} … ∞`, Math::Sequences' Hofstadter G
and H), which used to see an empty array and compute garbage (or deadlock on the sequence
state's lock). Each generated element is published to the list's cache at once, so the next step
of the same pull sees it. `Array.clone` of such an array pulls the generated tail from the
original (`LazyList::generator_origin`), as Rakudo's clone shares the reifier, so both arrays
hold the same random values in `roast/S32-array/create.t`.
