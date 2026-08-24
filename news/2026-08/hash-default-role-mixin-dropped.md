# Preserve role behavior when rendering Hash defaults

Fixed rendering of an `is default(...)` value on a typed Hash when the default is a native value mixed with a role that provides `Str`. The Hash had always retained the complete Mixin value; the loss occurred later when `say` used the native base value's `gist` without redispatching its virtual `Str` call through the Mixin.

Mixin-aware output now enters role dispatch before native rendering. A role-provided `gist` wins directly, while an inherited native `gist` correctly reaches a role-provided `Str`, so a missing-key read such as `0 but role :: { method Str { "NULL" } }` prints `NULL`.
