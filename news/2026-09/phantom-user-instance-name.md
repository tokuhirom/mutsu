# User classes no longer get a phantom `.name`

Generic user-class instances no longer answer `.name` unless the class declares
that accessor. Declared user attributes and built-in `.name` methods continue to
dispatch normally.
