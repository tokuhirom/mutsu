# A bare `emit` in a sub called by a Promise `whenever` reaches its supply

Promise callbacks dispatched by the VM react loop now restore the enclosing supply emitter for the callback's dynamic extent. A sub called from such a callback can use bare `emit` to send values to that supply, matching Rakudo.
