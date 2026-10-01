# `add` / `remove` on a user subclass of `BagHash`

`class MyBag is BagHash { }; MyBag.new.add("a")` died with "No such method 'add'". The
baggy-subclass delegate now forwards `add` and `remove` to the wrapped `BagHash`, adjusting its
counts in place through the shared node, exactly as a plain `BagHash` does.
