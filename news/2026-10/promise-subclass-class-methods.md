# `Promise.kept`/`broken`/`in`/`at`/`start`/`allof`/`anyof` on a subclass answer the subclass

Rakudo's `Promise` class methods build their result with `self.new`, so called
on a user subclass (`PRO.kept(3)`) they answer an instance of that subclass and
its own methods win (`PRO.kept(3).status` is the subclass's `status`). mutsu
answered a plain native promise that merely reported the subclass type. The
class-method rows now build the instance `new` makes and hand it the promise
the implementation produced, so the result keeps resolving as before. A
subclass instance is also accepted as an input to `Promise.allof`/`anyof`
(#14217).
