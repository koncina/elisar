# ElisaR

ElisaR can handle Tecan Sunrise excel exports (modified to include the plate layout and sample identifications). It will perform a 4PL regression (using the `drc` library) and return a dataframe containing the concentrations corresponding to the O.D. values.

## Known issues

During the testing phase using MS Excel 97-2003 files (*xls* extension), `readxl::read_excel()` sometimes imported cells containing text as "0.00" text values. Other issues are still [reported](https://github.com/hadley/readxl/issues) with `readxl` when dealing with Excel 97-2003 files, thus it is recommended to convert the Tecan Sunrise Excel files to the MS Excel 2007-2013 XML format (*xlsx* extension) after adding the layout and identification informations.
