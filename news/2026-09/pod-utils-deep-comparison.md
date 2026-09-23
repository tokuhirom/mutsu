# Pod::Utils object comparisons now match constructor shapes

mutsu now treats constructor-generated qualified attribute mirrors as part of
the same instance structure when their values equal the corresponding bare
attributes. This makes Pod::Utils 0.0.2's parser-created and constructor-created
Pod objects compare deeply equal while preserving differences in real inherited
or role-owned slots.
