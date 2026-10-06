# A zero Duration is now false

`Duration.new(0).Bool` answered `True` because the instance fell back to the default
truthiness of an object. `Duration` does `Real`, so `.Bool` is now delegated to its
underlying numeric value like the other numeric methods, and a zero `Duration` is false
as in Rakudo (#11989).
