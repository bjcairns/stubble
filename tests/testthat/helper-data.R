#===================#
#                   #
#### HELPER DATA ####
#                   #
#===================#


### Minimum Package Versions ###
min_v_bit64 = OPT_DEP[["bit64"]]
min_v_dt = OPT_DEP[["data.table"]]
min_v_tibble = OPT_DEP[["tibble"]]
  

### RNG Seed ###
use_seed <- 237892342L


### Common Test Objects ###
## Zero-Length Vectors ##
l0 <- list(
  character = character(0),
  Date = as.Date(character(0)),
  double = double(0),
  factor = factor(character(0)),
  IDate = if (getOption("stubble_has_data.table")) data.table::as.IDate(character(0)) else NULL,
  integer = integer(0),
  integer64 = if (getOption("stubble_has_bit64")) bit64::integer64(0) else NULL,
  ITime = if (getOption("stubble_has_data.table")) data.table::as.ITime(character(0)) else NULL,
  logical = logical(0),
  ordered = ordered(character(0)),
  POSIXct = as.POSIXct(character(0), tz = "UTC"),
  POSIXlt = as.POSIXlt(character(0), tz = "UTC")
)
l0 <- l0[!vapply(X = l0, FUN = is.null, FUN.VALUE = logical(1L))]
dat0 <- as.data.frame(l0)
suppressWarnings(
  if (getOption("stubble_has_data.table")) dt0 <- data.table::as.data.table(l0)
)
if (getOption("stubble_has_tibble")) df0 <- tibble::as_tibble(l0)

## Missing Data Vectors ##
lna <- list(
  character = NA_character_,
  Date = as.Date(NA_character_),
  double = NA_real_,
  factor = factor(NA_character_),
  IDate = if (getOption("stubble_has_data.table")) data.table::as.IDate(NA_character_) else NULL,
  integer = NA_integer_,
  integer64 = if (getOption("stubble_has_bit64")) bit64::as.integer64(NA_integer_) else NULL,
  ITime = if (getOption("stubble_has_data.table")) data.table::as.ITime(NA_character_) else NULL,
  logical = NA,
  ordered = ordered(NA_character_),
  POSIXct = as.POSIXct(NA_character_, tz = "UTC"),
  POSIXlt = as.POSIXlt(NA_character_, tz = "UTC")
)
lna <- lna[!vapply(X = lna, FUN = is.null, FUN.VALUE = logical(1L))]
datna <- as.data.frame(lna)
suppressWarnings(
  if (getOption("stubble_has_data.table")) dtna <- data.table::as.data.table(lna)
)
if (getOption("stubble_has_tibble")) dfna <- tibble::as_tibble(lna)

## Non-Zero-Length Vectors ###
l1 <- list(
  character = "",
  Date = as.Date("1970-01-01"),
  double = 0,
  factor = factor(""),
  IDate = if (getOption("stubble_has_data.table")) data.table::as.IDate("1970-01-01") else NULL,
  integer = 0L,
  integer64 = if (getOption("stubble_has_bit64")) bit64::as.integer64(0) else NULL,
  ITime = if (getOption("stubble_has_data.table")) data.table::as.ITime(0) else NULL,
  logical = F,
  ordered = ordered(""),
  POSIXct = as.POSIXct("1970-01-01", tz = "UTC"),
  POSIXlt = as.POSIXlt("1970-01-01", tz = "UTC")
)
l1 <- l1[!vapply(X = l1, FUN = is.null, FUN.VALUE = logical(1L))]
dat1 <- as.data.frame(l1)
suppressWarnings(
  if (getOption("stubble_has_data.table")) dt1 <- data.table::as.data.table(l1)
)
if (getOption("stubble_has_tibble")) df1 <- tibble::as_tibble(l1)

## Unique Vectors ##
luniq <- list(
  character = letters,
  Date = as.Date(1:1e2, origin = "1970-01-01"),
  double = seq(1, 1e2, 1),
  factor = factor(letters),
  IDate = if (getOption("stubble_has_data.table")) data.table::as.IDate(1:1e2, origin = "1970-01-01") else NULL,
  integer = 1:1e2,
  integer64 = if (getOption("stubble_has_bit64")) bit64::as.integer64(1:1e2) else NULL,
  ITime = if (getOption("stubble_has_data.table")) data.table::as.ITime(1:1e2) else NULL,
  logical = c(FALSE, TRUE),
  ordered = ordered(letters),
  POSIXct = as.POSIXct(1:1e2, origin = "1970-01-01", tz = "UTC"),
  POSIXlt = as.POSIXlt(1:1e2, origin = "1970-01-01", tz = "UTC")
)
luniq <- luniq[!vapply(X = luniq, FUN = is.null, FUN.VALUE = logical(1L))]

## Penguin Data Alias ##
penguins <- penguins_ext
