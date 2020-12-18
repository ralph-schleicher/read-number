# read-number

Reading numbers from an input stream without using the Lisp reader.


## Functions


**read-integer** (&optional _input-stream_ _eof-error-p_ _eof-value_ _recursivep_ &key _unsigned-number_ _plus-sign_ _minus-sign_ _group-separator_ _radix_)

Read an integer from an input stream.


**read-float** (&optional _input-stream_ _eof-error-p_ _eof-value_ _recursivep_ &key _unsigned-number_ _plus-sign_ _minus-sign_ _group-separator_ _decimal-point_ _exponent-marker_ _float-format_ _significand-radix_ _exponent-radix_ _exponent-base_)

Read a floating-point number from an input stream.


## Features

* Optional arguments _input-stream_, _eof-error-p_, _eof-value_, and
  _recursivep_ behave similar to `read`.

* Keyword argument _unsigned-number_ controls the sign conventions;
  plus or minus sign, minus sign only (no explicit plus sign), or
  unsigned.

* Keyword arguments _plus-sign_, _minus-sign_, _group-separator_,
  _decimal-point_, and _exponent-marker_ denote character sets.

* Keyword arguments _significand-radix_, _exponent-radix_, and
  _exponent-base_ provide support for reading, for example, C99
  hexadecimal floating-point literals.

* Leading and trailing whitespace is not ignored.

* Compatible with `with-input-from-string`.


## Commentary

The `read-integer` and `read-float` functions are designed to read
external number representations.  The `read-number` function (to be
defined) is reserved for reading any Lisp number representation.

The functions provided by [read-number][] are different to
[parse-number][] and [parse-float][] because they read characters
sequentially from a stream instead of parsing a string directly.


[read-number]: https://github.com/ralph-schleicher/read-number
[parse-number]: https://github.com/sharplispers/parse-number
[parse-float]: https://github.com/soemraws/parse-float
