## Reason for resubmission

Removed dependency on package mlr3proba that was removed from CRAN.
Apologies for not being able to upload a new version in time.

## R CMD check

Results in one NOTE:

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-05-16 as requires archived package 'mlr3proba'.

  The dependency on 'mlr3proba' has been removed in the updated version.


There is one NOTE that is only found on Windows (Server 2022, R-devel 64-bit):

```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

As noted in R-hub issue #503, this could be due to a bug/crash in MiKTeX and can likely be ignored.

- WARNINGs or ERRORs

## R-HUB

All checks show "Status: success"