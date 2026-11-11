# Element assignment through an rw method named apart from its attribute

`$obj.hh<k> = v` where `method hh() is rw { nqp::getattr(self, P, '%!h') }`
died "rw method 'hh' does not expose an assignable attribute": the in-place
element store only recognised an accessor named like its attribute. It now
also finds the instance's attribute by the identity of the container the
method handed back, so the element lands in that attribute, as in rakudo.
