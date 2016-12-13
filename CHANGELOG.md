0.7.1
-------

Call 'Snap.Core.pass' when routing an empty URI path. This allows an entire
served API to fall through, which is more in line with the rest of snap routing,
and allows multiple servant API's to be served under the same path context
from 'Snap.Core.route'.

0.7.0.5
-------

Fix throwError bug ignoring ServantError headers

0.7.0.4
-------

Fix throwError bug ignoring ServantError body

0.7.0.3
-------

Bump servant upper bound, allow 0.9

0.7
----

Initial release
