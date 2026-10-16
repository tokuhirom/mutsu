# A sigilless role parameter is a term in the role body

`role ProjectionModel[Mu:U \m] { method of { m } }` died at parse time with "Null regex not allowed": inside the role body `m }` was read as a match quote with `}` as the delimiter. Sub and method bodies already registered their sigilless parameters as terms; the role body now does too, so a bare `m`, `s`, `y` or `tr` that names a role parameter is that parameter. This was the parse failure that kept Red (`Red::ResultSeq`) from loading.
