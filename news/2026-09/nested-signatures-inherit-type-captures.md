# Nested signatures inherit type captures

The signature validator now carries type capture names from an enclosing sub into nested sub and multi signatures. Nested parameters and return types can use a type captured by their outer routine while an unrelated signature still rejects the unknown name.
