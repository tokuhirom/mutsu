# A grammar code block can read an attribute

A grammar rule's code block or code assertion that read an attribute
(`token t { a { say $!n } }`, `<?{ $!n }>`) died with "Variable $!n used where
no 'self' is available". The block got the rule's cursor as `self` only when
its text named `self`. It now also gets the cursor when it uses an attribute
twigil (`$!n`, `$.n`, `@!list`), since an attribute resolves against `self`.
`$!` alone is still the error variable. The start rule's blocks see the built
invocant `.parse` makes (#10848), so `<?{ $!n }>` there reads the attribute's
default; a subrule's cursor is minted without BUILD, so its attributes read
uninitialised, as in rakudo (#10730).
