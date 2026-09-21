---
title: "Support NativeCall's :types export tag"
---

mutsu now accepts `use NativeCall :types` and imports the NativeCall C type
objects through the same explicit tag as Rakudo. This lets distributions such
as `SSH::LibSSH::Tunnel` load their NativeCall bindings unchanged.
