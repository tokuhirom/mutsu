# A package routine's topic read takes the env shortcut

`GetGlobal` serves a free-variable read straight from `env` when no store
the slow chain asks first can hold the name. For a general name that needs
four things: no mainline capture, no routine-nested sub alias, no file-scope
bucket of a package-less module, and a GLOBAL current package (because of
package-block lexicals). So every `$_` read in a module's routines (in
`.map({ ... })`, `for`, `given`, `with`) walked the whole by-name chain. That
was the most frequent slow-chain read on the MCP::Server::Tool::Web
`html-to-text` probe.

The topic is every routine's own: none of those four stores ever holds it.
A `$_` read now takes the shortcut outside GLOBAL too. A debug assertion
checks that the stores ahead of `env` all miss the topic (#18274).
