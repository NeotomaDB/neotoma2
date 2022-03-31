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

The `neotoma2` R package is a tool to access and manipulate data from the Neotoma Paleoecology Database [@williams2018neotoma] within the R environment. The package uses the Neotoma API v2.0 [@goringapi] as a tool to import records from the Neotoma database, allowing researchers to examine taxonomic, spatial and temporal patterns across space and time over the last 5.4 million years.

# Statement of need

The `neotoma` R package [@goring2015neotoma] leveraged the Neotoma Paleoeocology Database v1.0 API.  Changes to the underlying database requires new data objects within the R package, to more closely align to the Neotoma data model [@grimm2018].

# Citations

Citations to entries in paper.bib should be in [rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html) format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
    - `@author:2001`  ->  "Author et al. (2001)"
    - `[@author:2001]` -> "(Author et al., 2001)"
    - `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:

![Caption for example figure.\label{fig:example}](figure.png) and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:

![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We acknowledge contributions from the Neotoma Paleoecology Community.

# References
