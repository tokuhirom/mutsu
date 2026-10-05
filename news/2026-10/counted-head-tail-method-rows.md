# Counted head and tail methods move to rows

The one-argument `Any.head` and `Any.tail` methods now use built-in method
rows for the receiver shapes covered by the table. Their handlers preserve
writable `Array` elements, and the native cascade reuses the same
implementations for receiver forms the table does not admit.
