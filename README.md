[![Build Status](https://travis-ci.org/DamienCassou/beginend.svg?branch=master)](https://travis-ci.org/DamienCassou/beginend)
[![Coverage Status](https://coveralls.io/repos/github/DamienCassou/beginend/badge.svg)](https://coveralls.io/github/DamienCassou/beginend)

# beginend

## Summary

Redefine M-< and M-> for some modes.

- in dired mode, M-< (resp. M->) go to first (resp. last) file line.
- in message mode,
   - M-< go to first line of message body (after headings)
   - M-> go to last line before message signature

## Installing

Use [melpa](https://melpa.org/).

In your Emacs init file, add:

```emacs
(beginend-setup-all)
```

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Damien Cassou.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
