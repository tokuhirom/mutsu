# The Acme::Cow shape: the file is `Acme/Cowish.rakumod`, so it is `use`d as
# `Acme::Cowish`, but the package it actually declares is `CowNS`. Nothing
# about the module's name says so -- which is the whole point of this fixture.
unit module CowNS;

class basic {
    has Str $.eyes is rw = "oo";
    method who { "basic" }
}

class cow is basic {
    method who { "cow" }
}
