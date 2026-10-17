Declared postfix operators whose names begin with `!` now take precedence over
private method parsing when their token matches the full `!method` spelling.
This works for both local declarations and imported operators, while ordinary
private method calls remain available.
