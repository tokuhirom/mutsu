# Native arrays declared in expressions retain their element width

An array declaration used as an expression now registers its native element type before storing the initializer and checks the elements as a statement declaration does. Values such as `say my uint8 @a = 1000, 2000` now wrap to `[232 208]`.
