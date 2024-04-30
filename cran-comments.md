## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release. We performed reverse dependency check for 9 packages.
As we change the standard output format, this will break the following packages:
- visOmopResults
- DrugUtilisation
- CohortSurvival
- CodelistGenerator
- CohortCharacteristics
- CDMConnector

This is expected and a new release of all this packages is ready to submit once 
this one is in CRAN. In most of the cases it only breaks tests and not the 
functionality. All maintainers are aware of it.
