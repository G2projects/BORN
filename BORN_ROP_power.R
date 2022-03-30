# --------------------------------------------------------------------#

# Title: Transfusion-induced Retinopathy Of Prematurity
# PI: Luciana Teofili, MD
# Developer: Fernando Palluzzi, PhD
# Centre: Policlinico Universitario Agostino Gemelli IRCCS
# Project started: 29/04/2021

# --------------------------------------------------------------------#


## Design

# Interventional, Blinded, Randomized, Controlled

# ROP stages:
#    0: absent
#  1-2: mild
#    3: moderate
#  3-5: severe

# Infant mortality: 15% over 2 years

# Goal: reduce the incidence of severe ROP as much as possible (virtually 0).

# Confounders:

#  - Incidence stratification by gestational age
#          24th week:    38%
#     24th-26th week: 15-20%
#     26th-28th week:     5%
  
#  - 6 involved centres


## Packages installation (unlock at the first run)
#install.packages("pwr")
#install.packages("samplesizeCMH")


## Power analysis considering the impact of severe ROP risk stratification.

# R

library(pwr)

#                24w  24-26w  26-28w
# n0             100     100     100
# Control         38      20       5   =  63/300  =  0.21
# Experimental    10       5       1   =  16/300  =  0.05
# reduction      -75%    -75%    -75%

ho <- 2*asin(sqrt(0.21)) - 2*asin(sqrt(0.05))  # overall h value
ho
# ho = 0.5010408; 0.5 corresponds to an intermediate effect size (Cohen, 1988).

# Proportion test
# 2*pwr.p.test(h, significanceLevel, power)$n   # Single proportion or, equivalently, ...
# pwr.2p.test(h, significanceLevel, power)$n    # Two proportions

# Safety
n.safe <- 2*pwr.p.test(h = ho, sig.level = 0.1, power = 0.8)$n

# Efficacy
n.eff <- 2*pwr.p.test(h = ho, sig.level = 0.05, power = 0.8)$n

# Including mortality ...
m <- 0.15
n.safe <- n.safe + m*n.safe   # = 2*(25 + 25*0.15)
n.eff <- n.eff + m*n.eff      # = 2*(31.3 + 31.3*0.15)

n.safe
# 58 subjects (i.e., rounded up to 58)

n.eff
# 72 subjects 


## CMH test for the evaluation of multicentricity impact.

library(samplesizeCMH)

n.centres <- 6

p1 <- rep(0.21, n.centres)
p2 <- rep(0.05, n.centres)

# s = 0.21 + 0.05 = 0.26
# s is rounded to 0.3

power.cmh.test(p1 = p1, p2 = p2, theta = NULL, N = NULL, s = 0.3,
               sig.level = 0.05,
               power = 0.8,
               alternative = "greater")

# N = 146 by calculation; effective N is equal to 150 subjects.

# N = 150 subjects covers both the possible effect of multicentricity and the 
# effect of severe ROP incidence stratification at different gestational ages.
