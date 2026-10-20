Proto routine and regex declarations now require a statement separator
before another declaration on the same line, including empty bodies. The
shared check uses the consumed source to distinguish an empty body from a
declaration without a body. It preserves routine postfixes and recognizes
Unicode line endings without mistaking embedded-comment text for a separator.

Fatal syntax errors inside proto bodies now remain errors instead of being
discarded by the raw-body recovery path.
