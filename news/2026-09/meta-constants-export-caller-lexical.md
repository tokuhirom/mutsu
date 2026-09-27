# META::constants now passes its custom EXPORT test suite

mutsu now preserves uppercase user-variable lexicals when capturing methods declared in a class. This fixes `META::constants` 0.0.6, whose `EXPORT` hook invokes a method on its `use` argument; the method reads a caller-defined `my constant %META`. The ecosystem record is now green, with all 17 assertions passing under mutsu and Rakudo.
