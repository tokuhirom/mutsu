# An interpolation inside user concat uses core string joining

String interpolation now joins pairs of ordinary `Str` parts through the core
string candidate. An explicit user `infix:<~>` can build its result with
interpolation without recursively calling itself for each part. Interpolation
still dispatches to a matching user candidate for an object part.
