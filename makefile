all: data cor plot func
data: data/fhs.rds
			data/sample.csv
			data/ffq_ex8_for_kr.csv
			data/FHS_Ex8_platepos_ID_key.xls
			data/FHS_Mir_Corrected_df16_Progenesis_eic_only.xlsx
			
cor: correlations/pescCorBeta.rds
	correlations/vegetarianCorBeta.rds
	correlations/veganCorBeta.rds
	correlations/heiCorBeta.rds
	correlations/sigCor.rds
	correlations/food_Grp.rds
	correlations/adjusted_correlations.rds
	
plot: plots/heat.pdf
	plots/CorCluster.png
	plots/eicClusters.pdf
	
func: scripts/functo.R

.PHONY: all data cor plot func



data/md.rds: scripts/reade.R func
	Rscript $<

#matrices/data frames

correlations/sigCor.rds: scripts/regresso.R data/md.rds func
	Rscript $< 
correlations/vegetarianCorBeta.rds: scripts/facto.R data/md.rds func
	Rscript $<
correlations/pescCorBeta.rds: scripts/facto.R data/md.rds func
	Rscript $<
correlations/veganCorBeta.rds: scripts/facto.R data/md.rds func
	Rscript $<
correlations/heiCorBeta.rds: scripts/score.R data/md.rds func
	Rscript $<
correlations/food_Grp.rds: scripts/foodCo.R data/md.rds func
	Rscript $<
correlations/adjusted_correlations.rds: scripts/bake.R data/md.rds func
	Rscript $<
	


plots/heat.pdf: scripts/heate.R data/md.rds func
	Rscript $<
plots/eicClusters.pdf: scripts/eicClusters.r data/md.rds func
	Rscript $<
	
	
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
	
	