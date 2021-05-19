

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
#' @examples hpop_df %>% scenario_precent_decrease(percent_decrease=25)
scenario_percent_baseline <-
  function(df,
           percent_decrease,
           value = "value",
           baseline = 2018,
           old_baseline = baseline,
           target_year = 2025,
           sname) {
    #generate scenario name
    if (hasArg(sname)) {

    }
    else if (baseline == old_baseline) {
      sname = paste0("scen_perc_", percent_decrease, "_", baseline)
    } else {
      sname = paste0("scen_perc_catchup_", percent_decrease, "_", old_baseline)
    }
    goal_df <- df %>%
      dplyr::filter(year == old_baseline) %>%
      dplyr::mutate(goal = !!sym(value) * (100 - percent_decrease) / 100,
             target_year = target_year) %>%
      dplyr::select(iso3, ind, goal, target_year)
    base_df <- df %>%
      dplyr::filter(year == baseline) %>%
      dplyr::select(iso3,
             ind,
             baseline = year,
             baseline_value = !!sym(value))
    df %>%
      dplyr::left_join(goal_df) %>%
      dplyr::left_join(base_df) %>%
      dplyr::mutate(
        !!sym(sname) := if_else(
          year >= baseline &
            year <= target_year,
          baseline_value + (goal - baseline_value) * (year - baseline) / (target_year -
                                                                            baseline),
          NA_real_
        )
      ) %>%
      dplyr::select (-baseline,-baseline_value, -target_year,-goal)
  }
