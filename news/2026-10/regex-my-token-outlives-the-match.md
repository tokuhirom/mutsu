# A `:my token` declared in a regex stays visible to later regexes in its scope

A `:my token T { … }` declarator inside a regex used to be registered for the
duration of the match and then rolled back, so a later `/ <T> /` in the same
block died. It now behaves like a statement-level `my token`: it stays
registered after the match and is dropped by the block-scope restore when the
enclosing block exits, matching Rakudo (#10267).
