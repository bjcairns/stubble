#=====================#
#                     #
#### INTERNAL DATA ####
#                     #
#=====================#


### Control List Parameter Classes ###
CTRL_CLASS <- list(
  character = c("rng_kind", "dttm_tz", "agn_chr_sym", "agn_chr_sep", "agn_fct_lvls", "agn_fct_use_lvls"),
  Date = c("agn_date_origin", "agn_date_min", "agn_date_max"),
  double = c("p_na", "agn_dbl_min", "agn_dbl_max", "emp_sw", "emp_tail_exc", "emp_fuzz_spl", "emp_p_exc", "emp_fuzz_samp"),
  integer = c("agn_int_min", "agn_int_max", "agn_int_list", "agn_dbl_round", "agn_dbl_signif", "agn_chr_min", "agn_chr_max", "agn_chr_try_unique_attempts", "agn_chr_duplicated_nmax", "emp_n_exc", "index"),
  integer64 = c("agn_int64_min", "agn_int64_max"),
  ITime = c("agn_time_min", "agn_time_max"),
  logical = c("agn_unique", "agn_chr_try_unique", "agn_fct_force_unique", "agn_lgl_force_unique", "emp_drop_lev"),
  POSIXct = c("agn_dttm_min", "agn_dttm_max")
)


### Optional Dependencies ###
OPT_DEP <- list(
  bit64 = "4.0.2",
  data.table = "1.9.8",
  tibble = "1.1"
)


### Output ###
save(
  CTRL_CLASS, OPT_DEP,
  file = "R/sysdata.rda",
  compress = "gzip",
  version = 2
)


### Tidy Up ###
rm(CTRL_CLASS, OPT_DEP)
