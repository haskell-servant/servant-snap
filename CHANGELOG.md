0.8.4
-----

Support servant 0.15 and 0.16, which have a new `Stream` combinator
Drop support for servant < 0.15


0.8.3
-----

Add support for servant-0.14
Reorder handling of errors

0.8.2
------

Add `HasServer` instances for `StreamGenerator`


0.8.0.1
-------

Add headers from MonadSnap state response to the servant-snap computed response
Add a commented-out snap-cors test to the test suite. It doesn't pass, although
manual testing of snap-cors works.

0.8
-------

Copy BasicAuth and Context from servant-server to support basic auth checking

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
