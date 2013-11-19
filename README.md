# erli_api_worker

**erli_api_worker** is the backend worker of the [erli][erli] URL shortener.

Its CRUD API is accessible via HTTP and through a simple binary protocol via TCP and uses mnesia as storage backend. The API is designed to be self-explanatory and -discoverable. However, here is a quick overview:

### HTTP API Endpoints and Applicable Methods

    /paths                          GET, HEAD, OPTIONS, POST
    /paths/{PATH_ID}                GET, HEAD, OPTIONS, DELETE
    /paths/{PATH_ID}/targets        GET, HEAD, OPTIONS
    /paths/{PATH_ID}/visits         GET, HEAD, OPTIONS
    /targets                        GET, HEAD, OPTIONS, POST
    /targets/{TARGET_ID}            GET, HEAD, OPTIONS, DELETE
    /targets/{TARGET_ID}/paths      GET, HEAD, OPTIONS
    /targets/{TARGET_ID}/visits     GET, HEAD, OPTIONS
    /visits                         GET, HEAD, OPTIONS
    /visits/{VISIT_ID}              GET, HEAD, OPTIONS

#### Interna

For collection types pagination is handled via the ``Range`` header. The data returned by the server is intended to suffice for client driven exploration.

Purging of unwanted or infringing content is community driven in the form of infringement flagging. Every DELETE request towards a path or target resource counts towards a configurable limit (15 by default). The limit is per target, since multiple paths may point to the same target. Once the limit is reached all paths pointing to said target are marked as banned, as is the target itself. All requests to banned resources are answered with a ``410 GONE``.
Note that the current implementation is "dumb". To avoid storing user-related data on the server there is no way to prevent users from abusing the flagging process.

### TCP API Endpoint

TODO

#### The Binary Protocol

TODO

[erli]: http://github.com/mwn/erli
