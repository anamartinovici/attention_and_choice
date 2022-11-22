# Publication info

This repository contains code and synthetic data for: 

Martinovici, Ana, Rik Pieters, and Tulin Erdem (2022), "Attention Trajectories Capture Utility Accumulation and Predict Brand Choice", *Journal of Marketing Research*, https://doi.org/10.1177/00222437221141052 

If you want to cite the content of this repository, please use: 

Martinovici, Ana (2022), "Reproducibility package for Martinovici, Pieters, and Erdem (2022)", DOI: https://doi.org/10.25397/eur.21603288

# Repository info

To improve the likelihood of numerical reproducibility, this repository contains a `makefile` which specifies code execution order.

Sections 1-3 of Baker (2020) provide an introduction to GNU Make (Mecklenburg 2004) and basic concepts relevant to using a `makefile`. If you want to learn more about `Make`, I can recommend these freely-available resources :

- https://merely-useful.tech/py-rse/automate.html

- https://stat545.com/make-test-drive.html

- https://makefiletutorial.com/

- https://www.gnu.org/software/make/manual/html_node/Simple-Makefile.html#Simple-Makefile

In order to build the targets specified in the `makefile`, you need to have the following installed on your device: `Make`, `R`, `RStudio`, `rstan`, and `tidyverse`.

### How to test if `Make` is available on your device

Open a terminal and type `which make`. If you see a message that says "which: no make" and then a series of paths, then you either do not have make installed, or `Make` is not added to Path (Environment Variables on Windows). If you get a message that shows a path to `bin/make`, then you have `Make` installed and added to Path. 

### How to test if you can use `Make` to compile `stan` code

After you clone the repository, open a terminal and check that the working directory is the root directory of the repository. In the terminal, type `make test_target`. The test is successful if it generates `test_setup/stan_test.rds` and `test_target`.

### How to build the targets

After you clone the repository, open a terminal and check that the working directory is the root directory of the repository. In the terminal, type `make`. This will start executing the code needed to produce the tables and figures that summarise results based on synthetic data. `output/current_results` contains these figures and tables. 

Note that building all targets is time consuming and computationally intensive (e.g., estimation of the attention model component takes 5-6 days on a Windows machine with 64GB of RAM and Intel Xeon CPU E5-1620 V3 3.5Hz). See `all_Make_steps.txt` for the sequence in which the scripts are executed and the arguments provided to each script.

# References 

Baker, P. (2020). Using GNU make to manage the workflow of data analysis projects.
*Journal of Statistical Software*, Code Snippets, 94 (1), 1–46.
https://doi.org/10.18637/jss.v094.c01

Mecklenburg, R. (2004). *Managing projects with GNU make* (3rd ed.). O’Reilly Media, Inc.
