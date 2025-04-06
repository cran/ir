## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6.5,
  fig.height = 3.5
)

## ----load-package-ir----------------------------------------------------------
library(ir)

## ----results='hide'-----------------------------------------------------------
ir_sample_data

## ----echo=FALSE---------------------------------------------------------------
rmarkdown::paged_table(ir_sample_data)

## -----------------------------------------------------------------------------
head(ir_sample_data$spectra[[1]])

## ----results='hide'-----------------------------------------------------------
d <- ir_sample_data
d$spectra[[1]] <- d$spectra[[1]][0, ]
d$spectra[[1]]

ir_normalize(d, method = "area")

## ----echo=FALSE---------------------------------------------------------------
d <- ir_sample_data
d$spectra[[1]] <- d$spectra[[1]][0, ]
d$spectra[[1]]

rmarkdown::paged_table(ir_normalize(d, method = "area"))

## -----------------------------------------------------------------------------
methods(class = "ir")

## ----results='hide'-----------------------------------------------------------
ir_sample_data[5:10, ]

## ----echo=FALSE---------------------------------------------------------------
rmarkdown::paged_table(ir_sample_data[5:10, ])

## -----------------------------------------------------------------------------
d1 <- ir_sample_data

class(d1[, setdiff(colnames(d), "id_sample")])

d1$spectra <- NULL
class(d1)

## -----------------------------------------------------------------------------
d2 <- ir_sample_data
d2$spectra[[1]] <- rep(d2$spectra[[1]], 2)
class(d2)

d3 <- ir_sample_data
colnames(d3$spectra[[1]]) <- c("a", "b")
class(d3)

## ----results='hide'-----------------------------------------------------------
library(dplyr)

d <- ir_sample_data

d <- 
  d |>
  mutate(a = rnorm(n = length(spectra)))
  
head(ir_sample_data)

## ----echo=FALSE---------------------------------------------------------------
library(dplyr)

d <- ir_sample_data

d1 <- 
  d |>
  mutate(a = rnorm(n = length(spectra)))
  
rmarkdown::paged_table(head(ir_sample_data))

## -----------------------------------------------------------------------------
library(purrr)
library(ggplot2)

d2 <- 
  d |>
  group_by(sample_type) |>
  summarize(
    spectra = {
      res <- map_dfc(spectra, function(.x) .x[, 2, drop = TRUE])
      spectra[[1]] |>
        dplyr::mutate(
          y =
            res |>
            rowwise() |>
            mutate(y = max(c_across(everything()))) |>
            pull(y)
        ) |>
        list()
    },
    .groups = "drop"
  )

plot(d2) + 
  facet_wrap(~ sample_type)

## ----results='hide'-----------------------------------------------------------
ir_sample_data |>
  slice(2) |>
  rep(20)

## ----echo=FALSE---------------------------------------------------------------
ir_sample_data |>
  slice(2) |>
  rep(20) |>
  rmarkdown::paged_table()

## -----------------------------------------------------------------------------
ir_sample_data |>
  slice(2) |>
  ir_subtract(y = ir_sample_data[3, ]) |>
  dplyr::mutate(id_sample = "subtraction_result") |>
  rbind(ir_sample_data[2:3, ]) |>
  plot() + 
  facet_wrap(~ id_sample)

## ----error=TRUE---------------------------------------------------------------
# This will not work
ir_sample_data |>
  slice(6) |>
  ir_add(y = ir_sample_data[3:4, ])

# but this will
ir_sample_data |>
  slice(2:6) |>
  ir_add(y = ir_sample_data[3, ]) 

## -----------------------------------------------------------------------------
ir_sample_data[2, ] + ir_sample_data[3, ]
ir_sample_data[2, ] - ir_sample_data[3, ]
ir_sample_data[2, ] * ir_sample_data[3, ]
ir_sample_data[2, ] / ir_sample_data[3, ]

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

