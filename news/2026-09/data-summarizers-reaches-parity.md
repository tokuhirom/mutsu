# Data::Summarizers reaches parity

Data::Summarizers 0.2.6 now passes all six of its ecosystem test files under
mutsu, improving from 24/38 to 38/38 assertions. Native Date and DateTime
constructors now decontainerize values read from Pair elements, and typed
hashes correctly autovivify Array values for chained positional assignment.

The same fixes also improve Data::Reshapers from 5/15 to 7/15 passing files.
