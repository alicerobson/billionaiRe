


#' Business as usual scenario
#'
#'  Scenario that uses existing estimates/projections for years
#'  after the baseline year
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param value Column name of column with indicator values.
#' @param baseline Year which is to be used as baseline for scenario (discuss Seth)
#' @param sname Scenario name. Defaults to `'scen_bau'` (discuss with Seth)
#'
#' @return Dataframe with additional scenario column
#' @export
#'
#' @examples hpop_df %>% scenario_bau()
scenario_bau <- function(df,
                         value = "value",
                         baseline = 2018,
                         sname = "scen_bau") {
  df %>% dplyr::mutate(!!sym(sname) := dplyr::if_else(year >= baseline, !!sym(value), NA_real_))
}

#
#' Scenario to reduce by a fixed percentage from a baseline value
#'
#' The scenario returned is often for a different period to the original scenario
#' For example: if the original scenario is to reduce the 2012 value by 30% by 2030
#'   then the scenario returned will be a portion of the straight line
#'  drawn from the baseline year value (eg 2018)  to the 2030 target (but it will
#'   only go out as far as the final year in the input data frame)
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param percent_decrease The percentage decrease that is to be acheived from value
#'            in baseline year by target_year
#' @param value Column name of column with indicator values.
#' @param baseline Year which is to be used as start of output scenario (discuss Seth)
#' @param old_baseline Year from which the percent decrease is measured
#' @param target_year Year by which the percent of decrease should eventually be acheived
#' @param sname Scenario name. Will be automatically constructed if not set (discuss with Seth)
#'
#' @return Dataframe with additional scenario column
#' @export
#'
#' @examples hpop_df %>% scenario_precent_decrease(percent_decrease = 25)
scenario_bau <- function(df,
                         value = "value",
                         baseline = 2018,
                         sname = "scen_bau") {
  df %>% mutate(!!sym(sname) := if_else(year >= baseline, !!sym(value), NA_real_))
}

#' Halt the rise scenario
#'
#' @param df
#' @param value
#' @param baseline
#' @param old_baseline
#' @param target_year
#' @param sname
#'
#' @return
#' @export
#'
#' @examples
scenario_halt <- function(df,
                          value = "value",
                          ind = "ind",
                          iso3 = "iso3",
                          year = "year",
                          baseline = 2018,
                          old_baseline = baseline,
                          target_year = 2025,
                          sname = NULL) {
  rootname = ifelse(baseline == old_baseline,
                    paste0("halt_", baseline),
                    paste0("catchup_halt_", old_baseline))
  sname = check_and_set_scenario_name(sname, rootname)
  # special case of percent_baseline
  df %>% scenario_percent_baseline(percent_decrease = 0,
                                   value = value,
                                   ind = ind,
                                   iso3 = iso3,
                                   year = year,
                                   baseline = baseline,
                                   old_baseline = old_baseline,
                                   target_year = target_year,
                                   sname = sname)
}

#' @noRd
calculate_goal <- function(value, year, startyear, perc) {
  startyear = max(min(year), startyear)
  value[year == startyear] * (100 - perc) / 100
}

#' @noRd
calculate_scenario_percent <- function(bv, g, y, b, ty) {
  ifelse(y >= b & y <= ty,
         bv + (g - bv) * (y - b) / (ty - b),
         NA_real_)
}

#' @noRd
calculate_baseline <- function(v, y, b) {
  v[y == b]
}

#' @noRd
check_and_set_scenario_name <- function(name, root) {
  if (is.null(name)) {
    name = paste0("scen_", root)
  }
  if (substring(name, 1, 5) != "scen_") {
    stop(sprintf("Scenario name must start with scen_"), call. = FALSE)
  }
  name
}

#' @noRd
calculate_fixed_target <- function(small_is_best,
                                   year,
                                   baseline,
                                   target_year,
                                   baseline_value)
{
  dplyr::case_when(
    small_is_best & year >= baseline & year <= target_year & baseline_value > target_value ~
      baseline_value + (target_value - baseline_value) * (year - baseline) / (target_year - baseline),
    small_is_best & year >= baseline & year <= target_year & baseline_value <= target_value ~
      baseline_value,
    !small_is_best & year >= baseline & year <= target_year & baseline_value < target_value ~
      baseline_value + (target_value - baseline_value) * (year - baseline) / (target_year - baseline),
    !small_is_best & year >= baseline & year <= target_year & baseline_value >= target_value ~
      baseline_value,
    TRUE ~ NA_real_
  )
}


#
#' Scenario to reduce by a fixed percentage from a baseline value
#'
#' The scenario returned is often for a different period to the original scenario
#' For example: if the original scenario is to reduce the 2012 value by 30% by 2030
#'   then the scenario returned will be a portion of the straight line
#'  drawn from the baseline year value (eg 2018)  to the 2030 target (but it will
#'   only go out as far as the final year in the input data frame)
#'
#' @param df Data frame in long format, where 1 row corresponds to a specific
#'     country, year, and indicator.
#' @param percent_decrease The percentage decrease that is to be acheived from value
#'            in baseline year by target_year
#' @param value Column name of column with indicator values.
#' @param baseline Year which is to be used as start of output scenario (discuss Seth)
#' @param old_baseline Year from which the percent decrease is measured
#' @param target_year Year by which the percent of decrease should eventually be acheived
#' @param sname Scenario name. Will be automatically constructed if not set (discuss with Seth)
#'
#' @return Dataframe with additional scenario column
#' @export
scenario_percent_baseline <- function(df,
                                      percent_decrease,
                                      value = "value",
                                      ind = "ind",
                                      iso3 = "iso3",
                                      year = "year",
                                      baseline = 2018,
                                      old_baseline = baseline,
                                      target_year = 2025,
                                      sname = NULL) {
  rootname = ifelse(
    baseline == old_baseline,
    paste0("perc_", percent_decrease, "_", baseline),
    paste0("perc_catchup_", percent_decrease, "_", old_baseline)
  )
  sname = check_and_set_scenario_name(sname, rootname)
  df %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    dplyr::mutate( "_goal_value" := calculate_goal(.data[[value]], .data[[year]], !!old_baseline, !!percent_decrease),
                   "_baseline_value" := calculate_baseline(.data[[value]], .data[[year]], !!baseline)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( !!sym(sname) := calculate_scenario_percent(.data[["_baseline_value"]], .data[["_goal_value"]], .data[[year]], !!baseline, !!target_year)) %>%
    dplyr::select(-c("_goal_value", "_baseline_value"))
}

#' @noRd
scenario_fixed_target <- function(df,
                                  value = "value",
                                  ind = "ind",
                                  iso3 = "iso3",
                                  year = "year",
                                  baseline = 2018,
                                  target_value,
                                  target_year,
                                  sname,
                                  small_is_best = FALSE) {
  # hit a single fixed target value, eg 18 beds or more, by targetyear
  check_and_set_scenario_name(sname, paste0("moveto_", target_value, "_baseline"))
  df %>%
    dplyr::group_by(.data[[ind]], .data[[iso3]]) %>%
    dplyr::mutate("_baseline_value" := calculate_baseline(.data[[value]], .data[[year]], !!baseline)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(!!sym(sname) := calculate_fixed_target(!!small_is_best, .data[[year]], !!baseline, !!target_year, .data[["_baseline_value"]])) %>%
    dplyr::select(-c("_baseline_value"))
}
