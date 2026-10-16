# Promise's class methods are method-table rows

`Promise.start`, `Promise.in`, `Promise.at`, `Promise.kept`, `Promise.broken`, `Promise.allof` and `Promise.anyof`
no longer answer through name-matching arms of the slow-path dispatcher: each is a row of the method table owned by
`Promise`, found for the `Promise` type object or a user subclass's (ADR-11276 §9.120).
