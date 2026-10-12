## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----read---------------------------------------------------------------------
library(marcxmlr)
library(dplyr)

source_xml <- system.file(
  "extdata",
  "orwell-marcxml.xml",
  package = "marcxmlr"
)

marc <- read_marcxml(source_xml)
marc

## ----schema-------------------------------------------------------------------
names(marc)

## ----repeated-subfields-------------------------------------------------------
marc |>
  filter(record_id == 1L, tag == "856") |>
  select(
    subfield_code,
    value,
    subfield_order,
    subfield_occurrence
  )

## ----repeated-fields----------------------------------------------------------
marc |>
  filter(record_id == 1L, tag == "650", subfield_code == "a") |>
  select(record_id, field_order, field_occurrence, value)

## ----title-query--------------------------------------------------------------
marc |>
  filter(tag == "245") |>
  select(record_id, subfield_code, value)

## ----complete-field-----------------------------------------------------------
marc |>
  group_by(record_id, field_order) |>
  filter(
    any(
      tag == "650" &
        subfield_code == "a" &
        value == "Totalitarianism"
    )
  ) |>
  ungroup() |>
  select(record_id, field_order, tag, subfield_code, value)

## ----analytical-table---------------------------------------------------------
titles <- marc |>
  filter(tag == "245", subfield_code == "a") |>
  transmute(record_id, title = value)

subjects <- marc |>
  filter(tag == "650") |>
  group_by(record_id, field_order) |>
  summarise(
    subject = paste(value, collapse = " ; "),
    .groups = "drop"
  ) |>
  group_by(record_id) |>
  summarise(
    subjects = paste(subject, collapse = " | "),
    .groups = "drop"
  )

left_join(titles, subjects, by = "record_id")

## ----edit---------------------------------------------------------------------
edited <- marc |>
  mutate(
    value = if_else(
      record_id == 1L &
        tag == "856" &
        subfield_code == "y" &
        subfield_occurrence == 2L,
      "Backup access",
      value
    )
  )

edited |>
  filter(record_id == 1L, tag == "856") |>
  select(subfield_code, value, subfield_occurrence)

## ----diagnose-----------------------------------------------------------------
diagnostics <- diagnose_canonical(edited)
diagnostics

## ----roundtrip----------------------------------------------------------------
out_xml <- tempfile(fileext = ".xml")
write_marcxml(edited, out_xml)

roundtrip <- read_marcxml(out_xml)
identical(edited, roundtrip)

## ----subset-roundtrip---------------------------------------------------------
selected <- marc |>
  filter(record_id == 1L)

selected_xml <- tempfile(fileext = ".xml")
write_marcxml(selected, selected_xml)

identical(selected, read_marcxml(selected_xml))

## ----recovery, eval=FALSE-----------------------------------------------------
# recovered <- read_marcxml(
#   "catalogue-with-malformed-records.xml",
#   on_marc_error = "skip",
#   error_report = "marc-errors.csv"
# )

## ----parquet, eval=requireNamespace("arrow", quietly = TRUE) && requireNamespace("XML", quietly = TRUE)----
parquet_dir <- tempfile("marcxmlr-parquet-")

marcxml_to_parquet(
  source_xml,
  output_dir = parquet_dir,
  workers = 1L,
  verbose = FALSE
)

catalogue <- arrow::open_dataset(parquet_dir)

catalogue |>
  summarise(rows = n()) |>
  collect()

## ----parquet-query, eval=requireNamespace("arrow", quietly = TRUE) && requireNamespace("XML", quietly = TRUE)----
catalogue |>
  filter(tag == "650", subfield_code == "a") |>
  count(value, sort = TRUE) |>
  collect()

## ----parquet-diagnostics, eval=requireNamespace("arrow", quietly = TRUE) && requireNamespace("XML", quietly = TRUE)----
diagnose_canonical(catalogue)

