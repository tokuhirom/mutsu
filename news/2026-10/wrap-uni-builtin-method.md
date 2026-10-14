# A .wrap of a built-in Uni method reaches NFD/NFC values

`Uni.^find_method('list').wrap(...)` is now honoured by `"ab".NFD.list`: the "wrapped or
augmented?" veto of the native method path looks the wrap chain up along the built-in class's
catalog MRO (`NFD` -> `Uni`), not only under the instance's own class name (#13011).
