## Reason for early submission

- Patch a bug in the prediction update logic
- Fix issues raised in https://cran.r-project.org/web/checks/check_results_mcboost.html

## Test Results

- Issues raised by CRAN checks have been adressed
- No new NOTEs, WARNINGs or ERRORs

## R-HUB
All r-hub checks pass without NOTEs, WARNINGs or ERRORs.
PREPERROR on Debian Linux, R-devel, GCC likely is likely due to https://github.com/r-hub/rhub/issues/448.


Solaris output with Vignettes:

```r
> rhub::check_on_solaris(check_args="")
> rhub::check_with_rdevel()
> rhub::check(platform="windows-x86_64-devel")
```

Return no NOTEs, WARNINGs or ERRORs