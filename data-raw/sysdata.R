#=====================#
#                     #
#### INTERNAL DATA ####
#                     #
#=====================#


### Control List Parameter Classes ###
CTRL_CLASS <- list(
  character = c("rng_kind", "agn_chr_sym", "agn_chr_sep", "agn_fct_lvls", "agn_fct_use_lvls", "dttm_tz"),
  Date = c("agn_date_origin", "agn_date_min", "agn_date_max"),
  double = c("p_na", "agn_dbl_min", "agn_dbl_max", "emp_sw", "emp_tail_exc", "emp_fuzz_spl", "emp_p_exc", "emp_fuzz_samp"),
  integer = c("agn_int_min", "agn_int_max", "agn_int_list", "agn_dbl_round", "agn_dbl_signif", "agn_chr_min", "agn_chr_max", "agn_chr_try_unique_attempts", "agn_chr_duplicated_nmax", "emp_n_exc", "index"),
  ITime = c("agn_time_min", "agn_time_max"),
  logical = c("agn_unique", "agn_chr_try_unique", "agn_fct_force_unique", "agn_lgl_force_unique", "emp_drop_lev"),
  POSIXct = c("agn_dttm_min", "agn_dttm_max")
)


### Output ###
save(
  CTRL_CLASS, MIN_V,
  file = "R/sysdata.rda",
  compress = "gzip",
  version = 2
)


### Tidy Up ###
rm(CTRL_CLASS)
