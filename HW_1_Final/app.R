# Title: Homework 1
# Author: Dominic Contreras
# Course: R Shiny for Operations Management
# Date: September 7, 2018

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

cars.load <- mtcars  # read in data
cars.load <- tibble::rownames_to_column(cars.load)  # convert row names to columns
colnames(cars.load)[1] <- "model"  # rename column containig car names
cars.load$disp <- NULL  # delete extra data
cars.load$drat <- NULL
cars.load$qsec <- NULL
cars.load$vs <- NULL
cars.load$am <- NULL
cars.load$gear <- NULL
cars.load$carb <- NULL
cars.load$wt <- cars.load$wt*1000  # convert weight to thousands

cars.load$model <- as.factor(cars.load$model)  # turn model into a factor
carnames <- as.character(unique(cars.load$model))  # create vector with car names

pdf(NULL)
