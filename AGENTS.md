# AGENTS.md

## Overview

This repository contains course materials and assignments for `R4CSS`, primarily as standalone `.Rmd` notebooks and local data files used for teaching and analysis exercises.

## Dev Commands

- Open the project in RStudio: `open R4CSS.Rproj`
- Render a specific notebook from R: `rmarkdown::render("AS04_join_edu_data.Rmd")`
- Check git status: `git status --short --branch`

## File Index

- `AS*.Rmd`: assignment notebooks
- `R*.Rmd`: lecture and concept notebooks
- `V*.Rmd`: visualization notebooks
- `W*.Rmd`: workshop notebooks
- `data/`: course datasets
- `images/`: image assets referenced by notebooks
- `private/`, `private_data/`: ignored local/private course materials

## Architecture Notes

- The project is a flat course workspace rather than a packaged R project.
- Most notebooks are self-contained and read local files directly.
- Data processing is typically performed inside each notebook instead of through shared helper modules.
- Generated caches and rendered artifacts should stay out of version control unless intentionally committed.

## Pending Tasks
- [ ] Complete the code and discussion sections in `AS04_join_edu_data.Rmd`.
---
*Last updated: 2026-03-29*
