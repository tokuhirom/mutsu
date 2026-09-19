# Exported method and sub declarations can share a name

Importing a module that exports both an instance method and a plain sub with
the same name no longer reports a redeclaration. The imported method remains
available for method dispatch while the plain sub keeps its callable binding.
