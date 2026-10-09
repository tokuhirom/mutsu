# The storage bridges leave the native-base probe lists

After the frame builder started ending a container subclass's deferral chain with a `Native` entry, the array, hash and baggy
storage bridges were no longer reached from the multi-candidate and no-frame probes either. They are removed from both, and
`NATIVE_BASE_NO_FRAME` is gone (ADR-11276 §9.43, part of #12387).
