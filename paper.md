---
  title: 'neotoma2: An R package to access data from the Neotoma Paleoecology Database'
tags:
  - R
  - paleoecology
  - databases
  - paleoenvironment
  - Holocene
  - Pleistocene
authors:
  - name: Socorro V. Dominguez
    orcid: 0000-0002-7926-4935
    affiliation: 1
  - name: Simon J Goring
    orcid: 0000-0002-2700-4605
    affiliation: 2
affiliations:
  - name: Independent Researcher
    index: 1
  - name: Department of Geography; University of Wisconsin -- Madison
    index: 2
date: 22 February 2022
bibliography: paper.bib
---

# Summary

The `neotoma2` R package is a tool to access and manipulate data from the Neotoma Paleoecology Database [https://neotomadb.org; @williams2018neotoma] within the R environment. Neotoma is a community curated paleoecological data resource, containing over 8 million unique observations, with global coverage, from 37 constituent databases. The package uses the Neotoma API v2.0 [@goringapi] as a tool to import records from the Neotoma database, allowing researchers to examine taxonomic, spatial and temporal patterns across space and time over the last 5.4 million years. The R package allows researchers to both download, and create new records using `get_` (e.g., `get_sites()`) and `set_` functions (e.g., `set_sites()`) respectively. This provides researchers with the opportunity to develop dynamic workflows that include data generated locally, not yet uploaded to Neotoma.

The `neotoma2` R package has been under dynamic development for over a year, but has been used for teaching and training [@EPDref2022]. This release of the `neotoma2` R package is intended to act as a clean release of the package, with all of the core features provided.

# Statement of Need

The `neotoma` R package [@goring2015neotoma] leveraged the Neotoma Paleoeocology Database v1.0 API.  Changes to the underlying database requires new data objects within the R package, to more closely align to the Neotoma data model [@grimm2018]. The broad user community for Neotoma [@Williams2018a;@Goring2018a] requires a toolset that can access and manage data for each of the more than 40 dataset types within Neotoma. This package conforms to a `tidyverse` [@wickham2019tidyverse] approach for data management, with data objects that more closely resemble the underlying data model within Neotoma (https://open.neotomadb.org/db_schema).

# Acknowledgements

We acknowledge contributions from the Neotoma Paleoecology Community, the participants of our workshops for the European Pollen Database, American Quaternary Association and the International Association of Limnologists/International Paleolimnology Association, and members of the EarthCube community. This work was funded through a grant to SJG from the National Science Foundation (NSF-1948926).

# References
