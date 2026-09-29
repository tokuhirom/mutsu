# X::NYI includes suggestions and workarounds in its message

`X::NYI.message` now includes the optional `did-you-mean` and `workaround` attributes on separate lines. The generic message also uses Rakudo's capitalization when no feature is supplied. The same text appears when the exception is stringified or thrown.
