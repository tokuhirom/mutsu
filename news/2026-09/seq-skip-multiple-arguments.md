`Seq.skip`, `List.skip`, and the inherited `Any.skip` now support the documented
multi-argument skip/produce patterns, including `Whatever` and lazy repeated
argument streams, without silently discarding arguments.
