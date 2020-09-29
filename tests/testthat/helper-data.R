#===================#
#                   #
#### HELPER DATA ####
#                   #
#===================#

### Common Test Objects ###
## Zero-Length Vectors ##
l0 <- list(
  character = character(0),
  Date = as.Date(character(0)),
  double = double(0),
  factor = factor(character(0)),
  IDate = if (stubble:::is.installed.package("data.table")) data.table::as.IDate(character(0)) else NULL,
  integer = integer(0),
  integer64 = if (stubble:::is.installed.package("bit64")) bit64::integer64(0) else NULL,
  ITime = if (stubble:::is.installed.package("data.table")) data.table::as.ITime(character(0)) else NULL,
  logical = logical(0),
  ordered = ordered(character(0)),
  POSIXct = as.POSIXct(character(0), tz = "UTC"),
  POSIXlt = as.POSIXlt(character(0), tz = "UTC")
)

## Missing Data Vectors ##
lna <- list(
  character = NA_character_,
  Date = as.Date(NA_character_),
  double = NA_real_,
  factor = factor(NA_character_),
  IDate = if (stubble:::is.installed.package("data.table")) data.table::as.IDate(NA_character_) else NULL,
  integer = NA_integer_,
  integer64 = if (stubble:::is.installed.package("bit64")) bit64::as.integer64(NA_integer_) else NULL,
  ITime = if (stubble:::is.installed.package("data.table")) data.table::as.ITime(NA_character_) else NULL,
  logical = NA,
  ordered = ordered(NA_character_),
  POSIXct = as.POSIXct(NA_character_, tz = "UTC"),
  POSIXlt = as.POSIXlt(NA_character_, tz = "UTC")
)

## Non-Zero-Length Vectors ###
l1 <- list(
  character = "",
  Date = as.Date("1970-01-01"),
  double = 0,
  factor = factor(""),
  IDate = if (stubble:::is.installed.package("data.table")) data.table::as.IDate("1970-01-01") else NULL,
  integer = 0L,
  integer64 = if (stubble:::is.installed.package("bit64")) bit64::as.integer64(0) else NULL,
  ITime = if (stubble:::is.installed.package("data.table")) data.table::as.ITime(0) else NULL,
  logical = F,
  ordered = ordered(""),
  POSIXct = as.POSIXct("1970-01-01", tz = "UTC"),
  POSIXlt = as.POSIXlt("1970-01-01", tz = "UTC")
)

luniq <- list(
  character = letters,
  Date = as.Date(1:1e2, origin = "1970-01-01"),
  double = seq(1, 1e2, 1),
  factor = factor(letters),
  IDate = if (stubble:::is.installed.package("data.table")) data.table::as.IDate(1:1e2, origin = "1970-01-01") else NULL,
  integer = 1:1e2,
  integer64 = if (stubble:::is.installed.package("bit64")) bit64::as.integer64(1:1e2) else NULL,
  ITime = if (stubble:::is.installed.package("data.table")) data.table::as.ITime(1:1e2) else NULL,
  logical = c(F, T),
  ordered = ordered(letters),
  POSIXct = as.POSIXct(1:1e2, origin = "1970-01-01", tz = "UTC"),
  POSIXlt = as.POSIXlt(1:1e2, origin = "1970-01-01", tz = "UTC")
)

penguins <- penguins_ext
