# `$proxy.VAR!private-method` returning `self` hands back the Proxy container

A private method called on `$proxy.VAR` runs with the Proxy container as `self`.
Returning `self` from it now yields the container, which a later `my $x = ...`
FETCHes, instead of the read-only `.VAR` object that stayed unfetched. This is
how AttrX::Mooish's `AttrProxy.new` builds its attribute proxies; its
`t/010-base.rakutest` and the other lazy-attribute files drive it.
