# packages versions

    Code
      resultPackageVersion(x)
    Message
      v CohortCharacteristics: 0.2.2
      v PatientProfiles: 1.2.0
      v visOmopResults: 0.3.0

---

    Code
      resultPackageVersion(x)
    Condition
      Warning:
      ! Multiple versions used for package PatientProfiles.
      i You can check the package_version with:
        omopgenerics::settings(<summarised_result>)
    Message
      v CohortCharacteristics: 0.2.2
      x PatientProfiles: 1.2.0; 1.1.0
      v visOmopResults: 0.3.0

