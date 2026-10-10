# Exception.throw and Failure.throw are method rows

The hand-written throw fast path is replaced by rows on Exception, X::AdHoc, X::TypeCheck::Assignment and Failure (ADR-11276 §9.54). Failure.throw now raises the wrapped exception.
