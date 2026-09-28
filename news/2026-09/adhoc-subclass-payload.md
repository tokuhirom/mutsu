# Inherit X::AdHoc payload access

Subclasses of `X::AdHoc` now retain the inherited `payload` accessor and use
the payload for their default message, gist, and string value. A subclass can
also call `$.payload` from its own `gist` method, as in the Raku exception
documentation.
