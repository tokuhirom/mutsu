The regex engine now memoizes the mark-stripped subject view and pattern used
by scoped `:ignoremark` subpatterns. Repeated matches over one subject no longer
rebuild the remaining subject for every atom invocation.
