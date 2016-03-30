# units-d
Units and Quantities of Measurement library for D.

This is a modified version of David Nadlinger's original library for working
with units of measurement in D.

Task List

- [x] Use new alias syntax

- [x] Use new enum syntax

- [x] Use new template syntax

- [x] Use UFCS

- [x] Use `AliasSeq` inplace of `TypeTuple`

- [x] Use `std.meta.Repeat` instead of `RepeatTypeTuple`

- [x] Qualify unittests with, when possible, `@safe pure nothrow @nogc`

- [x] Indentations according to Phobos code-standard

- [ ] Remove need for `makeIndexCtfe`

- [ ] Move `staticFind` and `IndexedTuple` to `std.meta`

- [ ] Replace `Curry` with `std.functional.applyLeft`.

- [ ] Make `ScaledUnit` with integer precision (such as 10 degrees) work with
trigonometric functions in `si.d`. Currently errors caused by incorrect cast in
`{Scaled|Affine}Unit.{from|to}Base`.

- [ ] Perhaps add `LinearUnit` and use it to express mappings between, for
instance, Celsius and Fahrenheit.

- [ ] Refactor `GetConversion` to do a breadth-first search instead of current
depth-first.

Original discussion at: http://forum.dlang.org/thread/io1vgo$1fnc$1@digitalmars.com?page=1

This alternative implementation contains unit expression parsing which might be
of interest aswell:
https://github.com/biozic/quantities/issues/2#issuecomment-150809715
