# WrapHandle and HyperConfiguration methods are rows

`Routine::WrapHandle.restore` and `HyperConfiguration`'s `batch` and `degree` are now rows of the one method table
(ADR-11276 §9.160). Before, the method-call entry matched each by name. One owner lookup now finds them, and it
also finds `WalkList`'s rows, which had an owner block of their own.
