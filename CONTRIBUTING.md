+ The Issues section is the primary communication channel, thus post issues, questions, feature requests there.
+ Pull requests are welcome.
+ To contribute **documentation** consider extending, fixing or adjusting the vignettes or function documentation.
+ To contribute **tests**:
    1. Restore the database backups in `inst/extdata` in your system to set up the test data sets. You can use either `.travis.yml` or `appveyor.yml` for reference. Alternatively the `inst/extdata/restore-data.sh` script might help.
    2. Use the `testthat` package for writing tests.
    3. Test set up goes into `helper_` scripts. Test teardown go into the `test_z_teardown.R` script. Unfortunately testthat 1.0.2 doesn't doesn't properly handle teardown methods.
    4. If you add new data to the test database, make sure that you also create a backup and push it to GitHub. For creating a backup, the `inst/extdata/backup-data.sh` script might help.
+ To contribute **code**:
    1. Look into the outstanding Issues and Milestones to get an idea where to start.
    2. Function names are in `lowerCamelCase`.
    3. Exported functions live in separate files. Supporting (unexported) functions are in `utils_`.
    4. Document functions with `roxygen2` tags. Document a lot.
    5. Consider using code formating tools (e.g. `formatR`) for a clean look. (will mess up SQL though...)
