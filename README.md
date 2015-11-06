# Component

This is a direct port of https://github.com/stuartsierra/component for Pixie.

There are only 2 minor differences:

* systems are regular maps and can only be declared via `system-map`
* pixie's records do not allow to add/use undeclared fields, so you
  must declare your "locals" in the record definition (it's better
  like this anyway).

The rest is 100% compatible.

## Installation

With [dust](https://github.com/pixie-lang/dust), add the following to
your project.edn `:dependencies`:

```clojure
[mpenet/component "0.1.1-alpha"]
```

## Copyright and License

The MIT License (MIT)

Copyright © 2015 Stuart Sierra

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
