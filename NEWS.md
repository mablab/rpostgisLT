# News

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Changed
- NEWS formatted according to [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
- tests are now based on `testthat`


## [0.5.0] - 2017-06-20
### Changed
- example datasets added (Roe deer GPS tracking dataset)

### Added
- `as_pgtraj`: new argument `tzone` allows for time zone specification, and 
applies the database time zone if left NULL (formerly used the R (system) time zone)

### Fixed
- `as_pgtraj`: fixed bug potentially affecting sorting of steps within 
a burst in `step_geometry` view, and subsequently in imported 
`ltraj` using `pgtraj2ltraj`.
- `pgtraj2ltraj`: infolocs columns of type `POSIXlt` now re-import time zone 
attribute correctly
- `ltraj2pgtraj`: fixed bug affecting infolocs factors with commas

## [0.4] - 2016-11-01
**INITIAL CRAN RELEASE**

### Changed
- Added handling of infolocs
- Added support for Type I (no time recorded) trajectories
- New pgtraj and burst summary views
- faster conversion to pgtraj from database data in as_pgtraj()

## [0.3] - 2016-08-23
### Changed
- Add pgTrajVacuum(), pgTrajDrop()
- Add ltraj2pgtraj(overwrite = TRUE/FALSE)
- Add dl_opt(rnames=TRUE/FALSE)
- The traj database model has changed by adding r_rowname, r2n, rel_angle to the table 'step'.
- Remove pgTrajR2TempT().
- Renaming <pgtraj_name>_step_geom –> <pgtraj_name>_step_geometry; <pgtraj_name>_params –> <pgtraj_name>_parameters

## [0.2.3] - 2016-08-10
### Changed

- Depends on adehabitatLT package.
- Add <pgtraj_name>_step_geom view for visualizing pgtrajes in QGIS.
- The traj database model has changed, including field and table name changes.
- Remove pgDropTempT().


## [0.2.2] - 2016-08-01
### Changed

- Bugfix version, no changes visible to the user.

[Unreleased]: https://github.com/mablab/rpostgisLT/tree/dev
[0.5.0]: https://github.com/mablab/rpostgisLT/releases/tag/v0.5.0
[0.4]: https://github.com/mablab/rpostgisLT/releases/tag/v0.4
[0.3]: https://github.com/mablab/rpostgisLT/releases/tag/0.3
[0.2.3]: https://github.com/mablab/rpostgisLT/releases/tag/0.2.3
[0.2.2]: https://github.com/mablab/rpostgisLT/releases/tag/0.2.2

