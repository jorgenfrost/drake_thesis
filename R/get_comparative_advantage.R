#' This function calculates RpcA (RCA_CAP) and rca (RCA) from international
#' trade data and population data.
#'
#' @param trade_data data frame: export data in long country-year-product
#' format.
#' @param pop_data data frame: population data in long country-year format.
#' @param metric character: specificy normalization metric as either "RpcA"
#' or "RCA" (caps does not matter).
#' @return a tidy tibble with RCA or RpcA values for each country-year-product
#' combination.
#' @export

get_comparative_advantage <- function(trade_data, pop_data, metric = "RpcA", mean_value = FALSE) {

  # Check inputs -------------------------------------------

  # Trade data
  if (is.data.frame(trade_data)) {
    hs96_tbl <- trade_data
  } else {
    stop("Error: `trade_data` is not a data frame.")
  }

  # Population data
  if (is.data.frame(pop_data)) {
    pop_tbl <- pop_data
  } else {
    stop("Error: `pop_data` is not a data frame.")
  }

  metric <- stringr::str_to_lower(metric)
  if (!metric %in% c("rpca", "rca")) {
    stop("Error: `metric` is not 'rpca' or 'rca'.")
  }

  if (!is.logical(mean_value)) {
    stop("Error: `mean_value`is a logical (TRUE or FALSE).")
  }

  if (metric == "rpca") {

    # RCA_cap is defined as:
    # (country c export in product p / country c population) / (global export in product p / global population)

    # The simplest approach is then to use four variables:
    # 1. export of country c in product p in a year (this is already the "export_value" col)
    # 2. the population of country c in a year (this will be the "pop")
    # 3. export of all countries in product p in a year (this will be "global_product_export")
    # 4. the global population in a year (this will be "global_pop")

    # global product export for each year
    product_totals <- hs96_tbl %>%
      group_by(hs_product_code, year) %>%
      summarize(
        global_product_export = sum(export_value)
      )

    # global pop for each year
    global_pop <- pop_tbl %>%
      group_by(year) %>%
      summarize(
        global_pop = sum(pop)
      )

    # join variables and calculate rca_cap as define above

    rca_cap_tbl <- hs96_tbl %>%
      left_join(pop_tbl) %>%
      left_join(product_totals) %>%
      left_join(global_pop) %>%
      mutate(
        rca_cap = (export_value / pop) / (global_product_export / global_pop)
      ) %>%
      select(
        country_code,
        year,
        hs_product_code,
        export_value,
        pop,
        rca_cap
      )

    if (mean_value == TRUE) {
      # This section calculates instead the RCA_CAP over the entire periods. I just take the
      # avg rca_cap for each country over all the years. Thhis is to avoid that
      # "low_complexity" country enters a product and drastically changes its complexity.

      rca_cap_tbl <- rca_cap_tbl %>%
        group_by(country_code, hs_product_code) %>%
        summarize(
          pop = mean(pop),
          export_value = mean(export_value),
          rca_cap = mean(rca_cap)
        )
    }

    return(rca_cap_tbl)
  } else if (metric == "rca") {

    # RCA is defined as:
    # (country c export in product p / country c export in all products) / (export of all countries in product p / export of all countries in all products)

    # The simplest approach is then to have four variables:
    # 1. export of country c in product p in a year (this is already the "export_value" col)
    # 2. export of country c in all products in a year (this will be "country_total_export")
    # 3. export of all countries in product p in a year (this will be "global_product_export")
    # 4. export of all countries in all products (this will be "global_total_export")

    # export of country c in all products in a year ("country_total_export")
    country_totals <- hs96_tbl %>%
      group_by(country_code, year) %>%
      summarize(
        country_total_export = sum(export_value)
      )

    # export of all countries in product p in a year ("global_product_export")
    product_totals <- hs96_tbl %>%
      group_by(hs_product_code, year) %>%
      summarize(
        global_product_export = sum(export_value)
      )

    # global total export for each year
    global_total_export <- hs96_tbl %>%
      group_by(year) %>%
      summarize(
        global_total_export = sum(export_value)
      )

    # join variables and calculate rca as define above
    rca_tbl <- hs96_tbl %>%
      left_join(country_totals) %>%
      left_join(product_totals) %>%
      left_join(global_total_export) %>%
      mutate(
        rca = (export_value / country_total_export) / (global_product_export / global_total_export)
      ) %>%
      select(
        country_code,
        year,
        hs_product_code,
        export_value,
        rca
      )

    if (mean_value == TRUE) {
      # This section calculates instead the RCA over the entire periods. I just take the
      # avg rca_cap for each country over all the years. This is to avoid that some
      # "low_complexity" country enters a product and drastically changes its complexity.
      rca_tbl <- rca_tbl %>%
        group_by(country_code, hs_product_code) %>%
        summarize(
          export_value = mean(export_value),
          rca = mean(rca)
        )
    }

    return(rca_tbl)
  }
}
