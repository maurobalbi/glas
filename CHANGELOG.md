# Changelog

## v0.2.1

### Fixed
- Free-standing files now provide hover and completions instead of crashing the server
- Fixed an ordering problem, where files were sometimes loaded into the LSP before all dependencies were downloaded

## v0.2.0

### Fixed
- Support for module constants
- Hover-documentation for qualified definitions
- Don't show private definitions in completions
- Add Info Message if 'gleam' is not in PATH

## v0.1.9

### Fixed
- Fix qualified imported Constructors inference
- Fix labelled field resolution
- Add tuple indexing inference

## v0.1.6

### Fixed
- Restrict renaming to local definitions
- Properly parse panic as expressions and labelled discard parameters
- Correctly reference labels
- Restrict nonsensical  renames

## v0.1.0

### New Features

- Renaming

### Improved

- Improve Reference for Variants, add better and more tests
- Render Documentation for Custom Types

## v0.0.9

### Added

- Added auto-format feature (uses gleam-binary)

### Changed

- Persist gleam diagnostics while typing

### Fixed

- Correctly infer Labelled arguments and fields.
- Correctly complete and hover field access expressions.
- Instantiate generic parameters.
