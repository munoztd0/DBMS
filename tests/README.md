Tests and Coverage
================
11 September, 2024 12:02:34

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                              | Coverage (%) |
|:----------------------------------------------------|:------------:|
| DbVieweR                                            |      0       |
| [R/app_config.R](../R/app_config.R)                 |      0       |
| [R/app_server.R](../R/app_server.R)                 |      0       |
| [R/app_ui.R](../R/app_ui.R)                         |      0       |
| [R/fct_helpers.R](../R/fct_helpers.R)               |      0       |
| [R/golem_utils_server.R](../R/golem_utils_server.R) |      0       |
| [R/golem_utils_ui.R](../R/golem_utils_ui.R)         |      0       |
| [R/mod_about.R](../R/mod_about.R)                   |      0       |
| [R/mod_create_table.R](../R/mod_create_table.R)     |      0       |
| [R/mod_del_rows.R](../R/mod_del_rows.R)             |      0       |
| [R/mod_graph_table.R](../R/mod_graph_table.R)       |      0       |
| [R/mod_import_table.R](../R/mod_import_table.R)     |      0       |
| [R/mod_insert_rows.R](../R/mod_insert_rows.R)       |      0       |
| [R/mod_modify_row.R](../R/mod_modify_row.R)         |      0       |
| [R/mod_update_table.R](../R/mod_update_table.R)     |      0       |
| [R/mod_view_table.R](../R/mod_view_table.R)         |      0       |
| [R/run_app.R](../R/run_app.R)                       |      0       |
| [R/utils_helpers.R](../R/utils_helpers.R)           |      0       |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning |
|:---|---:|---:|---:|---:|---:|---:|
| [test-fct_helpers.R](testthat/test-fct_helpers.R) | 1 | 0.037 | 0 | 0 | 0 | 0 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R) | 13 | 0.079 | 0 | 0 | 0 | 0 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R) | 51 | 0.143 | 0 | 0 | 0 | 0 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R) | 10 | 5.240 | 0 | 0 | 0 | 0 |
| [test-utils_helpers.R](testthat/test-utils_helpers.R) | 1 | 0.012 | 0 | 0 | 0 | 0 |

<details closed>

<summary>

Show Detailed Test Results
</summary>

| file | context | test | status | n | time |
|:---|:---|:---|:---|---:|---:|
| [test-fct_helpers.R](testthat/test-fct_helpers.R#L2) | fct_helpers | multiplication works | PASS | 1 | 0.037 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L2) | golem_utils_server | not_in works | PASS | 2 | 0.019 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L7) | golem_utils_server | not_null works | PASS | 2 | 0.012 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L12) | golem_utils_server | not_na works | PASS | 2 | 0.011 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L17_L22) | golem_utils_server | drop_nulls works | PASS | 1 | 0.016 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L26_L29) | golem_utils_server | %\|\|% works | PASS | 2 | 0.006 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L37_L40) | golem_utils_server | %\|NA\|% works | PASS | 2 | 0.005 |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L48_L50) | golem_utils_server | rv and rvtl work | PASS | 2 | 0.010 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L2) | golem_utils_ui | Test with_red_star works | PASS | 2 | 0.021 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L10) | golem_utils_ui | Test list_to_li works | PASS | 3 | 0.010 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L22_L28) | golem_utils_ui | Test list_to_p works | PASS | 3 | 0.010 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L53) | golem_utils_ui | Test named_to_li works | PASS | 3 | 0.009 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L66) | golem_utils_ui | Test tagRemoveAttributes works | PASS | 4 | 0.009 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L82) | golem_utils_ui | Test undisplay works | PASS | 8 | 0.016 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L110) | golem_utils_ui | Test display works | PASS | 4 | 0.008 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L124) | golem_utils_ui | Test jq_hide works | PASS | 2 | 0.006 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L132) | golem_utils_ui | Test rep_br works | PASS | 2 | 0.005 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L140) | golem_utils_ui | Test enurl works | PASS | 2 | 0.017 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L148) | golem_utils_ui | Test columns wrappers works | PASS | 16 | 0.026 |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L172) | golem_utils_ui | Test make_action_button works | PASS | 2 | 0.006 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3) | golem-recommended | app ui | PASS | 2 | 0.102 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13) | golem-recommended | app server | PASS | 4 | 0.029 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26) | golem-recommended | app_sys works | PASS | 1 | 0.008 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42) | golem-recommended | golem-config works | PASS | 2 | 0.015 |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L72) | golem-recommended | app launches | PASS | 1 | 5.086 |
| [test-utils_helpers.R](testthat/test-utils_helpers.R#L2) | utils_helpers | multiplication works | PASS | 1 | 0.012 |

</details>

<details>

<summary>

Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.3.1 (2023-06-16) |
| Platform | x86_64-pc-linux-gnu (64-bit) |
| Running  | Ubuntu 23.10                 |
| Language | en_US                        |
| Timezone | Asia/Tokyo                   |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.1   |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>

<!--- Final Status : pass --->
