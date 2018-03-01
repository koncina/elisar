# elisar 0.2.9000

* Rewrote functions to import plates: Using base R and better keyword detection (`which` instead of `for` loops).
* Limited import dependencies to `readxl`
* Added `extract_elements()` which returns a list of all detected elements.
* `read_plate()` replaces `read.plate()` and produces a `data.frame` out of the data/layout/id elements found in the file(s). Multiple data elements are supported and merged with the layout and ID table (should be unique among all files supplied to `read_plate()`)
* Added support for additional plate sizes (6, 12, 24, 48 and 96 well plate). Mixing different sizes is not supported yet.

# elisar 0.2

* On stale branch `v0.2`: started to upgrade to new tidyverse tools (purrr).

# elisar 0.1

* Added a `NEWS.md` file to track changes to the package.



