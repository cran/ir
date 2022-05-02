## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6.5,
  fig.height = 3.5
)

## ----setup, include=FALSE-----------------------------------------------------
library(kableExtra)

## ----load-package-ir----------------------------------------------------------
library(ir)

## ----import-csv-table-format, echo=FALSE--------------------------------------
read.csv("../inst/extdata/klh_hodgkins_mir.csv") %>%
  dplyr::select(1:5) %>%
  dplyr::slice(1:6) %>%
  kableExtra::kable()

## ----import-csv-1-------------------------------------------------------------
d_csv <- ir_import_csv("../inst/extdata/klh_hodgkins_mir.csv", sample_id = "from_colnames")

## -----------------------------------------------------------------------------
library(dplyr)
library(stringr)

# import the metadata
d_csv_metadata <- 
  read.csv("./../inst/extdata/klh_hodgkins_reference.csv",
           header = TRUE,
           as.is = TRUE) %>%
  dplyr::rename(
    sample_id = "Sample.Name",
    sample_type = "Category",
    comment = "Description",
    holocellulose = "X..Cellulose...Hemicellulose..measured.",
    klason_lignin = "X..Klason.lignin..measured." 
  ) %>%
  # make the sample_id values fir to those in `d_csv$sample_id` to make combining easier
  dplyr::mutate(
    sample_id =
      sample_id %>%
      stringr::str_replace_all(pattern = "( |-)", replacement = "\\.")
  )

d_csv <- 
  d_csv %>%
  dplyr::full_join(d_csv_metadata, by = "sample_id")

## ----import-spc-1-------------------------------------------------------------
d_spc <- ir_import_spc("../inst/extdata/1.spc", log.txt = FALSE)

## ----export-csv-1-------------------------------------------------------------
# export only the spectra
ir_sample_data %>%
  ir_flatten() %>%
  write.csv(tempfile("ir_sample_data_spectra", fileext = "csv"), row.names = FALSE)

# export only the metadata
ir_sample_data %>%
  ir_drop_spectra() %>%
  write.csv(tempfile("ir_sample_data_metadata", fileext = "csv"), row.names = FALSE)

## ----plot-1-------------------------------------------------------------------
plot(d_csv)

## -----------------------------------------------------------------------------
library(ggplot2)

plot(d_csv) + 
  geom_path(aes(color = sample_type))

## -----------------------------------------------------------------------------
plot(d_csv) + 
  geom_path(aes(color = sample_type)) +
  labs(x = expression("Wavenumber ["*cm^{-1}*"]"), y = "Absorbance") +
  guides(color = guide_legend(title = "Sample type")) +
  theme(legend.position = "bottom")

## ----preprocessing-before-1---------------------------------------------------
plot(d_spc)

## ----preprocessing-bc-1-------------------------------------------------------
d_spc %>%
  ir_bc(method = "rubberband") %>%
  plot()

## ----preprocessing-normalization-1--------------------------------------------
d_spc %>%
  ir_normalize(method = "area") %>%
  plot()

## ----preprocessing-normalization-2--------------------------------------------
d_spc %>%
  ir_normalize(method = 1090) %>%
  plot() +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = 1090, linetype = 2)

## -----------------------------------------------------------------------------
d_spc %>%
  ir_smooth(method = "sg", p = 3, n = 91, m = 0) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_smooth(method = "sg", p = 3, n = 9, m = 1) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_clip(range = data.frame(start = 1000, end = 3000)) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_interpolate(dw = 1) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_interpolate(dw = 1) %>%
  ir_normalize(method = 1090) %>%
  plot() +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = 1090, linetype = 2)

## -----------------------------------------------------------------------------
d_spc %>%
  ir_interpolate_region(range = data.frame(start = 1000, end = 3000)) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_bin(width = 30) %>%
  plot()

## -----------------------------------------------------------------------------
d_spc %>%
  ir_interpolate(dw = 1) %>%
  ir_clip(range = data.frame(start = 700, end = 3900)) %>%
  ir_bc(method = "rubberband") %>%
  ir_normalise(method = "area") %>%
  ir_bin(width = 10) %>%
  plot()

## ---- echo=FALSE--------------------------------------------------------------
sessionInfo()

