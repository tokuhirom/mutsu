# Propagate tap callback failures from Supplier.emit

An exception thrown by a live tap's emit callback now propagates to the caller of `Supplier.emit` rather than invoking that tap's `quit` handler. Explicit source quits still invoke the handler.
