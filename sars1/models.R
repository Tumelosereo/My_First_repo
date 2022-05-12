library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


# Source the functions script

source("./sars1/functions.R", local = TRUE)

# Read Parameter values for SARS Cov 2.

pp <- read_excel("./sars1/sars_cov1_par.xlsx", sheet = "sars_cov1_par")

# Extract only values from the Data Frame

parm_values <- pp$Value
names(parm_values) <- pp$Code
parm_values <- parm_values[1:11]

## State variables

N0 <- 150000 # Assumed total population

ss <- read_excel("./states.xlsx", sheet = "states")

# Extract only values from the Data Frame

state_var <- ss$Value
names(state_var) <- ss$Code


## Time interval

time_seq <- seq(0, 365, 1)

library(dplyr)

# Source the inputs

#source("./scripts/inputs.R", local = TRUE)

## Run the model to calculate cumulative

CM1 <- run_covid(
  t = time_seq,
  pp = parm_values,
  vals = c(
    ps = 0.25,
    du = 12,
    cc = 20
  ),
  statev = state_var
)

## We define new parameters with different values of
## ps, a and du.

## expand.grid helps to create the data with matching number of length.

new_vals <- expand.grid(
  ps = seq(0, 1, 0.01),
  du = 8:30,
  cc =  seq(20, 300, 20)
)

# Run the model to produce all rates from the COVID

out <- run_covid(
  t = time_seq,
  pp = parm_values,
  vals = c(ps=.65, du = 10, cc = 20),
  statev = state_var, ret_cm = FALSE
)

# Save simulation output



## Here we only run with 1 rows from data frame new_vals.

run_covid(
  t = time_seq,
  pp = parm_values,
  vals = new_vals[3, ],
  statev = state_var
)

## Calculating Cumulative Mortality for all values of (new_vals)

if (file.exists("./sars1/cm_res.RData")) {
  load("./sars1/cm_res.RData")
} else {
  cm_res <- sapply(1:nrow(new_vals), function(ii) {
    run_covid(
      t = time_seq,
      pp = parm_values,
      vals = new_vals[ii, ],
      statev = state_var
    )
  })
  save(cm_res, file = "./sars1/cm_res.RData")
}



## Combine used parameters with corresponding cm value and save it

new_df <- cbind(new_vals, cm = cm_res)

save(new_df, file = "./sars1/new_df.RData")

# Plot the simulation output for Ebola
# Make the data(out) long and plot according to the states.

new_out <- out %>%
  pivot_longer(S:CA, values_to = "Count", names_to = "States")

model_out <- (ggplot(new_out)
              +
                geom_line(aes(
                  x = time,
                  y = Count,
                  col = States
                ), size = 1)
              +
                labs(
                  x = "Time(days)"
                  , y = "Population"
                  #, title = "Probability of surviving is 75%, 12 days of hospitalization
                   # with 20 availabe beds in population of 150000 individuals."
                )
              +
                facet_wrap(~States, scales = "free") 
              +
                ggeasy::easy_center_title()
              +
                theme_bw()
              
)

print(model_out)



########################################################
# Hypothetical drugs for Ebola virus

bed_capacity <- 20

baseline_cm <- function(bed_cap, psb = .62, dub = 10) new_df %>%
  filter(ps == psb, du == dub, cc == bed_cap)

plot1 <- (ggplot()
          +
            aes(x = ps, y = du, z = cm, color = as.character(cc)
            )
          +
            geom_contour(breaks = c(min(new_df$cm), baseline_cm(bed_capacity)$cm, max(new_df$cm)), data = new_df[new_df$cc == bed_capacity, ])
          +
            geom_contour(breaks = c(min(new_df$cm), baseline_cm(180)$cm, max(new_df$cm)), data = new_df[new_df$cc == 180, ])
          +
            geom_point(data = baseline_cm(bed_capacity), size = 3, col = "blue")
          +
            lims(x = c(0, 1))
          +
            geom_vline(aes(xintercept = ps), data = baseline_cm(bed_capacity))
          +
            geom_hline(aes(yintercept = du), data = baseline_cm(bed_capacity))
          +
            labs(#title = "Cumulative mortality at given number of beds",
              x = "Probablilty of surving (given hospitalized)"
              , y = "Hospital duration (days)"
              , color = "Number of beds")
          + 
            theme_bw()
          +
            ggeasy::easy_center_title()
          
)


#######################################################
#filter according to the know drug base line
# We have duration of stay and probability of surviving

no_of_beds <- 20

treat_area<- c(
  xmin = c(.68), xmax = c(.88),
  ymin = c(15), ymax = c(28)
)

(ggplot(new_df[new_df$cc == no_of_beds, ])
  + aes(x = ps, y = du, z = cm)
  + geom_contour_filled()
  + annotate("rect", xmin=c(treat_area[1]), xmax=c(treat_area[2]),
             ymin=c(treat_area[3]), ymax=c(treat_area[4]),
             alpha=0.2, color="white", fill="white")
)

bed_capacity <- 100

baseline_cm <- function(bed_cap, psb = .75, dub = 15 )new_df %>%
  filter(ps ==psb, du == dub, cc == bed_cap)

(ggplot()
  + aes(x = ps, y = du, z = cm, color = as.character(cc))
  + geom_contour(breaks = c(min(new_df$cm),baseline_cm(bed_capacity)$cm, max(new_df$cm)),
                 data = new_df[new_df$cc == bed_capacity,])
  + geom_contour(breaks = c(min(new_df$cm),baseline_cm(20)$cm, max(new_df$cm)),
                 data = new_df[new_df$cc == 20,])
  + geom_contour(breaks = c(min(new_df$cm),baseline_cm(120)$cm, max(new_df$cm)),
                 data = new_df[new_df$cc == 120,])
  + geom_contour(breaks = c(min(new_df$cm),baseline_cm(200)$cm, max(new_df$cm)),
                 data = new_df[new_df$cc == 200,])
  + lims(x = c(0,1))
  + geom_point(data = baseline_cm(bed_capacity), size = 2, col = "red")
  + geom_hline(aes(yintercept = du), data = baseline_cm(bed_capacity))
  + geom_vline(aes(xintercept = ps), data = baseline_cm(bed_capacity))
)

#######################################


(ggplot(new_df[new_df$ps == .25, ])
 + aes(x = cc, y = cm)
 + geom_line()
 
)
######################################

## Plotting different figures

(ggplot(new_df[new_df$ps == .25,])
 + aes(x = cc, y = cm)
 + facet_wrap(~du)
 + geom_line()
 + ylim(0,1500)
)

#############################################

(ggplot(new_df, aes(x = cc, y = ps, z = cm))
 + facet_wrap(~du)
 +geom_contour_filled()
 
)

##############################################

(ggplot(new_df[new_df$cc == 200,])
 + aes(x = ps, y = du)
 + geom_line()
)
