# "Must be implemented by" names classes and roles without their site keys

The `Method 'x' must be implemented by C because it is required by roles: R.` compile error printed the
declaration-site key of a lexical class or role (`FooR1 3`). It now uses the user-facing names, as rakudo
does. Fixes #14540.
