# A Supplier ended inside the react body now ends the react

`react { whenever $s.Supply { ... }; $s.emit(1); $s.done }` hung forever. `Supplier.done`
reset the supplier's terminal state and buffered emits at once, but the react registers its
sink only after its body finished, so the replay of `done` found nothing. The reset is now
deferred while a react's setup hold is on the supplier and runs when the last hold is released.
