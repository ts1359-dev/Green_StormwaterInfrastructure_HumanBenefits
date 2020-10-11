# Green Stormwater Infrastructure Benfits on Humans - evaluate what factors control whether or not green stormwater infrastructure provides net human benefits ( NB = 1) or not (NB = 0) using income, rainfall, and the dummy variable Rural as possible X variables.

The data provided for this assignment is a n by 5 matrix, where n is the number of green stormwater infrastructure elements (biofilters) evaluated.
  •HB is a vector that illustrates the magnitude of benefit people could receive from each of the n different biofilters  (positive HB = lots of benefit, negative HB = little benefit).
  •NB is a binary vector where infrastructure elements that provide positive net benefit receive a score of 1 and infrastructure elements that provide negative net benefit receive a score of 0. 
  •Income is the average household income (dollars/week) for the community where each biofilter is located
  •Rainfall is the average rainfall in mm for the community where each biofilter is located
  •Rural is a proxy variable for whether or not there is available land that isn’t constrained by urban development. If large areas of land are available for green stormwater infrastructure, Rural = 1. If space is highly constrained and only small tracts of unpaved land are available for green stormwater infrastructure, Rural = 0.
