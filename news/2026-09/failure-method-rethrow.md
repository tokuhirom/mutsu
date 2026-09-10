# Unhandled Failure method calls rethrow their exception

Method calls through a variable now rethrow the exception carried by an
unhandled `Failure` before native dispatch can treat it as an ordinary value.
Handling and introspection accessors, including `defined`, `Bool`, `so`,
`.^name`, and `.exception`, remain available.
