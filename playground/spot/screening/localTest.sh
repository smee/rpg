#!/bin/bash
for PBS_ARRAYID in {1..18}; do clear; export PBS_ARRAYID; Rscript localTest.R; done;
