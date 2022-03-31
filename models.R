library(ggplot2)
library(dplyr)
library(readxl)

# # Read Parameter values for SARS Cov 2.

pp <- read_excel("sars_cov2_par.xlsx", sheet = "sars_cov2_par")

# Extract only values from the Data Frame

parm_values <- pp$Value
names(parm_values) <- pp$Code

## State variables

N0 <- 150000 # Assumed total population

ss <- read_excel("states.xlsx", sheet = "states")

# Extract only values from the Data Frame

state_var <- ss$Value
names(state_var) <- ss$Code

## Time interval

time_seq <- seq(0, 365, 1)

## Parameter values.

parm_values <- c(alpha = 1/6,     # 1/duration of pre-symptomatic = 1/6 days
                 eta = 1/7,       # 1/duration of asymptomatic to recover = 1/7 days
                 sigma = 1/5,     # NB: for now (NCEM)
                 epsilon = 1/3,   # 1/duration of infection to hospitalized = 1/3 days
                 #mu = 1/12,       # progression rate from H_T(d): 1/12 days
                 nu = 1/12,       # 1/duration from not hospital to death = 1/15 days
                 gamma = 1/18,    # 1/18 days=1/duration from hospital to recovered
                 pa = 0.75,        # prob of remaining asymptomatic 75% (PHIRST-C)
                 pm = 0.95,       # prob of being mild or moderate given symptomatic 90% (NCEM)
                 #ps = 0.25,       # prob of survival given hospitalization (DATCOV)
                 #a = 10,          # Hospital capacity constrain:-(cc)
                 b = 1,
                 pmax = 0.4,      # Maximum probability of being hospitalized in cc
                 # 40% proportion of severe ILI cases.
                 brn = 2,          # Reproduction number
                 D = 5)           # Duration of infection in days


## State variables

N0 = 150000  # Assumed total population

state_var <- c(S = N0-1,
               Ia = 1,
               Is = 0,
               M = 0,  
               V = 0,
               Nt = 0,
               Hts = 0,
               Htd = 0,  
               R = 0,
               CM = 0,
               CA = 0)

## Time interval

time_seq <- seq(0, 356, 1)

## Run the model to calculate cumulative

CM1 <- run_covid(t = time_seq,
                 pp = parm_values,
                 vals =  c(ps = 0.25,
                           du = 12,
                           cc = 10),
                 statev = state_var)

## We define new parameters with different values of
## ps, a and du.

## expand.grid helps to create the data with matching number of length.

new_vals <- expand.grid(ps = seq(0, 1, 0.05),
                        du = 8:30,
                        cc = seq(20, 300, 20))

## Here we only run with 1 rows from data frame new_vals.

run_covid(t = time_seq,
          pp = parm_values,
          vals =  new_vals[1,],
          statev = state_var)

## Calculating Cumulative Mortality for all values of (new_vals)
if (file.exists("cm_res.RData")) {
  load("cm_res.RData")
} else {
  cm_res <- sapply(1:nrow(new_vals), function(ii) {
    run_covid(
      t = time_seq,
      pp = parm_values,
      vals = new_vals[ii, ],
      statev = state_var
    )
  })
  save(cm_res, file = "cm_res.RData")
}

## Combine used parameters with corresponding cm value.

new_df <- cbind(new_vals, cm = cm_res)
#######################################################
#filter according to the know drug base line
# We have duration of stay and probability of surviving

no_of_beds <- 160

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
  + geom_point(data = baseline_cm(bed_capacity), size = 5, col = "red")
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