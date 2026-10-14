# A role stub satisfied by an accessor and an explicit method is one candidate

A parent class that declares both `has %.charset` and explicit
`multi method charset` candidates counted the accessor as a second inherited
provider of a subclass's required `method charset {...}`, so composing the
role failed with "multiple inherited candidates". The explicit method replaces
the generated accessor, so it is now a single candidate. PDF::Font::Loader's
`Enc::Type1`, `FontObj` and `FontObj::CID` (and through them the main module)
now load; the dist's own suite needs native FreeType/OpenSSL libraries rakudo
cannot load here either.
