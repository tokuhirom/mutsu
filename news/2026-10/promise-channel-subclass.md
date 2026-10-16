# A user subclass of Promise or Channel is a real promise or channel

`class P is Promise { ... }; await P.new` died with "No such method 'get-await-handle'": an instance of a user
subclass was a second, attribute-based imitation of a promise that knew only `keep`, `result`, `status` and `then`.
The instance now carries a native promise (or channel), and every built-in method answers on it, so `await`, `.then`,
`.vow`, `.cause`, `.receive`, `.List` and the rest behave as on Rakudo, while the subclass's own methods and attributes
keep working (#13634, ADR-11276 §9.119).
