## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release. We performed reverse dependency check for 9 packages.
As we change the standard output format, this will break the following packages:
- visOmopResults
- DrugUtilisation
- CohortSurvival
- CodelistGenerator
- CohortCharacteristics

This is expected and a new release of all this packages is ready to submit once 
this one is in CRAN.
We submitted a new version of CDMConnector yesterday that should work with this 
release. But the old one will break.
