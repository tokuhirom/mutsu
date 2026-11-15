An `is raw` method that exposes an attribute bound to an outer scalar now
preserves the scalar's container when assigning through the method. The
compiler records the binding's container capture, and the lvalue assignment
writes through the attribute's existing cell so both names observe later
writes.
