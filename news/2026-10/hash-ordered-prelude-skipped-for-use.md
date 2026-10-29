# The built-in `Hash::Ordered` prelude is not injected over the zef module

A compunit that `use`s or `need`s `Hash::Ordered` means the distribution's
`role Hash::Ordered`; injecting mutsu's built-in class there redeclared the
symbol as soon as a second compunit did the same (BSON::Simple's test helper,
#15427). The prelude is still injected for code that only names the type.
