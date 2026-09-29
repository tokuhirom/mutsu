# Gist of a list of objects is truncated at 100 elements

`dir`'s Seq (and any list/Seq holding objects such as `IO::Path`) rendered every
element in `.gist`, because the method-dispatching gist path had no element cap.
It now stops after 100 elements and appends ` ...`, like Rakudo and mutsu's pure
gist renderer. Closes #9847.
