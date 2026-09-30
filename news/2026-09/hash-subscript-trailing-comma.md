# A trailing comma inside a `{...}` hash subscript now parses

`%h{"a",}` was rejected with "Confused" because the subscript parser only accepted a trailing
comma before `]` or `;`. It now also accepts one before `}`, giving a one-element slice like
`@a[0,]` does (#10040).
