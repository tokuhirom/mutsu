# `STORE` of `is Hash`/`is Map` instances and mixins goes through the rows

The `STORE` name arm of `call_method_with_values` is gone. A user `is Hash`/`is Map` instance and a
mixin over a hash or array now reach `Hash.STORE`/`Array.STORE` with their backing storage as the
detached place (`native_container_store`), so there is one implementation of `STORE` (ADR-11276
§9.176).
