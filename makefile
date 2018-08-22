all: data model plot
data: raw.csv
model: model.Rout
plot: plot.Rout

.PHONY: all data model plot

clean:
  rm -f raw.csv clean.csv/
  rm -f model.Rout/
  rm -f plot.Rout

## data         : download the data
raw.csv: get_data.py
    python get_data.py

## clean.csv    : clean the data
clean.csv: clean.sh raw.csv
    source clean.sh

## model       : run linear models
model.Rout: model.R clean.csv
    R CMD BATCH model.R

## plot        : plot model coefs
plot.Rout: plot.R model.Rout
    R CMD BATCH plot.R

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<