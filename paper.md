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
  - name: Socorro Dominguez Vidaña
    orcid: 0000-0002-7926-4935
    affiliation: 1
  - name: Simon J Goring
    orcid: 0000-0002-2700-4605
    affiliation: "2, 3"
affiliations:
  - name: HT Data
    index: 1
  - name: Department of Geography, University of Wisconsin -- Madison
    index: 2
  - name: Center for Climatic Research, University of Wisconsin -- Madison
    index: 3
date: 22 February 2022
bibliography: paper.bib
---

# Summary

The `neotoma2` R package is a tool to access and manipulate data from the Neotoma Paleoecology Database (https://www.neotomadb.org) within the R environment. Neotoma is a community curated paleoecological data resource [@williams2018neotoma], containing nearly 9 million unique observations of paleoecological proxies with global coverage from 37 constituent databases. The package uses the Neotoma API v2.0 [@goring2023api] as a tool to import records from the Neotoma database, allowing researchers to examine taxonomic, spatial and temporal patterns across space and time over the last 5.4 million years. The R package allows researchers to download, and create new records using `get_*()` (e.g., `get_sites()`) and `set_*()` functions (e.g., `set_sites()`) respectively. This provides researchers with the opportunity to develop dynamic workflows that include data generated locally, and not yet submitted to the Neotoma database.

The `neotoma2` R package has been under dynamic development for over a year, but has been used for teaching and training [@Goring2023APD]. This release of the `neotoma2` R package is a clean release of the package, with all of the core features provided and extensive test coverage implemented.

# Statement of Need

The `neotoma` R package [@goring2015neotoma] leveraged the Neotoma Paleoeocology Database v1.0 API and had been one of the primary tools for researchers working with data from Neotoma [@wang2023plants;@kujawa2016effects; @byun2021extensive]. Changes to the underlying database and a rebuilding of the API required new data objects within the R package to more closely align to the Neotoma data model [@grimm2008neotoma]. The broad user community for Neotoma [@williams2018neotoma; @goring2018nexus] requires a toolset that can access and manage data for each of the more than 40 dataset types within Neotoma and so extensive metadata must be accessed for each record. This package conforms to a `tidyverse` [@wickham2019tidyverse] approach for data management, with data objects that more closely resemble the underlying data model within Neotoma (https://open.neotomadb.org/db_schema). Most importantly the `neotoma2` package provides a toolset for paleoecologists, ecologists, conservation ecologists, archaeologists, and others, to access and examine the broad range of fossil data contained within the Neotoma Paleoecology Database.

Data from the Neotoma Database can be accessed through the public API (https://api.neotomadb.org) or a Postgres database snapshot with a database client [@williams2018neotoma] or through the EarthLife Consortium API [@uhen2021earthlife]. The R package will simplify many of the operations required to assemble and manipulate datasets, and provides functions thatsupport researchers as part of their analytic workflows by linking Neotoma directly to packages within R used for ecological and earth science research, data visualization and statistical analysis.

# Acknowledgements

We acknowledge contributions from the Neotoma Paleoecology Community, the participants of our workshops for the European Pollen Database, American Quaternary Association and the International Association of Limnologists/International Paleolimnology Association, and members of the EarthCube community. This work was funded through a grant to SJG from the National Science Foundation (NSF-1948926).

# References
