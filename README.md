# agilecalendar

An R Shiny app to generate a (SAFe) Agile cadence calendar based on a configuration file in which (repeating)
markers can be specified that are then plotted on the calendar as a guide when a scrum master, release train
engineer, or PO, or any other agile team member can or needs to act.

# Installation 

`agilecalendar` is not available on CRAN and can only be installed using `remotes`.

```r
remotes::install_github('paullemmens/agilecalendar')
```

# Usage

The package's most exposed functionality is its Shiny app used to visualize the agile calendar although this
can also be done in an R console using the plotting functionality of the package.

```r
## Start the dashboard to access and load the package default configuration.
run_app()

## Manually create and plot the calendar.
## fn <- 'path/config.yml'
cfg <- read_config(fn)
cal <- generate_calendar(cfg)
plot_calendar(cal)
```

# Agile configuration

The YAML configuration comprises three parts. The first, `configuration` contains a series of strings,
numbers, and dates to set up the agile calendar. `year` is merely a string that is used for prefixing the year
identifier to various labels. `year_start` should be the date when the agile year starts and the `..._length`
parameters are the durations of particular periods in weeks.

The next section `agile_events` contains, typically, repeating events that are relevant to the user of the
package. Currently, the repetition needs to be entered by the user. 

`markers` is similar in setup but is intended for one off markers that are perhaps not necessarily directly
related to the agile cadence.

Fundamentally, at this stage of the package, there is no particular difference between the last two sections
regarding which events they (should) cover, except perhaps the color in which they are visualized.

# Todo

[ ] Make agile events simpler by enabling referring to agile weeks, so they repeate automatically.
[ ] Add calendar view on the agile calendar.

# Help

Use github for reporting bugs and suggestions for improvement.
