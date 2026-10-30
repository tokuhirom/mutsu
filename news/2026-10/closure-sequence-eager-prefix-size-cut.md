# An unbounded closure sequence cuts its eager prefix short when elements get expensive

`2, { $_**2 - $_ + 1 } ... *` pre-computed 32 elements when the sequence was
created, so a generator whose values grow doubly exponentially (Sylvester's
sequence, as in `Math::Sequences`) hung on bigints, and a look-and-say string
sequence built megabyte strings. The eager warm-up prefix now stops once an
element exceeds 4096 bits / bytes and the lazy continuation computes the rest on
demand (#15306). `.lazy` on such a sequence also kept no generator history; it
does now.
