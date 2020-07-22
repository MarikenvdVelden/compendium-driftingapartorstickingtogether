# Drifting Apart or Sticking Together? An analysis of party platform changes in 8 Western European countries
Data &amp; Analysis Compendium for the Drifting Apart or Sticking Together? An analysis of party platform changes in 8 Western European countries paper.

# Data
The [integrated data](data/intermediate/cleaned_dyadic_data.csv) integrates, tidies and pre-processes the [Manifesto Project Database](https://manifesto-project.wzb.eu/datasets){:target="_blank"}, ParlGov </a>, <a href = "https://erdda.org/cpd/" target="_blank" > the European Representative Democracy Data Archive </a>, and [opinion poll data sets](data/raw/poll_data.csv) collected by the authors. This file is used to test the three hypotheses on the conditions under which parties stick together or drift apart after a term in office. Additionally, the data contains all additional variables to conduct all the robustness checks reported in the Supporting Information. See the scripts in [src/data-processing](src/README.md) for details on how the data is constructed.

# Results
* [Results](src/analysis/01_main_analysis.md)
* [Results of Robustness Checks](src/analysis/02_robustness_checks.md)
