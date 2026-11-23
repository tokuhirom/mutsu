# A method frame adopts its class's statics instead of inserting them

Every compiled method call copies its class's class-body `my` statics into its
fresh frame overlay, so the method body (and any closure it creates) sees
them. That copy was one env insert per static per call. AttrX::Mooish's
role body declares four statics, and its methods run about twenty times per
lazy-attribute round.

The statics plan, already kept per class until the statics tables change, now
also holds those writes applied to a shared overlay tier. A frame whose
overlay has had no write yet adopts that tier outright. Its first write of its
own copies it, so the frame sees exactly what the inserts built and the shared
tier is never changed through a frame (#18274).
