echo '\nStarting replication.\nAll output will be stored in data/summResults directory.\nTable and Figure numbers from paper are referenced when needed.'

# rm -rf data/summResults/*.png
# rm -rf data/summResults/*.tex
# rm -rf data/summResults/*.pdf

echo '\nBuild data files.'

Rscript replicationSetup.R

echo '\nGenerate Figure 2.'

Rscript other/dvNet.R

echo '\nRunning models (ERGM, LSM, LSM (Bilinear), Logit, MRQAP, AMEN).\nOutput for various model types stored in data/rdaResults\nand diagnostic plots stored in data/summResults.\nThis takes about two hours.'

Rscript runMods/runERGM.R 2>&1 >/dev/null
Rscript runMods/runLatentSpace.R 2>&1 >/dev/null
Rscript runMods/runLatentSpaceBil.R 2>&1 >/dev/null
Rscript runMods/runLogit.R 2>&1 >/dev/null
Rscript runMods/runQAP.R 2>&1 >/dev/null
Rscript runMods/runAMEN.R 2>&1 >/dev/null

echo '\nGenerate Table 4.\nOutput stored in data/summResults/regTable.tex.'

Rscript runResults/coefCompare.R 2>&1 >/dev/null

echo '\nGenerate Figure 3.\nOutput stored in data/summResults/ameFitSR_2_UV.pdf.'

Rscript runResults/latSpaceViz.R 2>&1 >/dev/null

echo '\nRun out-of-sample performance analysis.\nThis will take several hours and will monopolize all available cores on your machine.\nIf desired, reset clusters to utilize on line 10 of outPerf/runMods_wMiss.R.\nOutput for various model types stored in data/rdaResults.'

Rscript outPerf/runMods_wMiss.R 2>&1 >/dev/null

echo '\nGenerate Figure 4.\nOutput stored in data/summResults/:\n\troc_outSample.pdf\n\trocPr_outSample.pdf\n\tVarious separation plots corresponding to model type\n\taucTable_outSample.tex.'

Rscript outPerf/outSampPerf.R 2>&1 >/dev/null

echo '\nGenerate Figure 4.\nOutput stored in data/summResults/netPerfCoef.pdf.'

Rscript runResults/netPerfSecondary.R 2>&1 >/dev/null 

echo '\nPaper Table/Figure replication complete.\nStarting on appendix.\n\n-------------\n'

echo '\nGenerate Figure A1.\nOutput stored in data/summResults/ameConv2_SR2.pdf.'

Rscript runResults/amenModelCompare/checkConv_amen.R 2>&1 >/dev/null 

echo '\nGenerate Figure A2.\nOutput stored in data/summResults/ggGofAll.pdf.'

Rscript runResults/netPerf.R 2>&1 >/dev/null 

echo '\nGenerate Table A2.\nOutput stored in data/summResults/regTable_latSpace.tex.'

Rscript runResults/latSpaceModelCompare/coefCompare_latSpace.R 2>&1 >/dev/null 

echo '\nGenerate Figure A3.\nOutput stored in data/summResults/:\n\troc_latSpace_outSample.pdf\n\trocPr_latSpace_outSample.pdf\n\tVarious separation plots corresponding to model type\n\taucTable_latSpace_outSample.tex.'

Rscript outPerf/outSampPerf_latentSpace.R 2>&1 >/dev/null 

echo '\nGenerate Figure A4.\nOutput stored in data/summResults/netPerfCoef_latSpace.pdf.'

Rscript runResults/latSpaceModelCompare/netPerfSecondary_latSpace.R 2>&1 >/dev/null 

echo '\nGenerate Table A3.\nOutput stored in data/summResults/regTable_ameSR.tex.'

Rscript runResults/amenModelCompare/coefCompare_amen.R

echo '\nGenerate Figure A5.\nOutput stored in data/summResults/:\n\troc_ameSR_outSample.pdf\n\trocPr_ameSR_outSample.pdf\n\tVarious separation plots corresponding to model type\n\taucTable_ameSR_outSample.tex.'

Rscript outPerf/outSampPerf_amen.R 2>&1 >/dev/null 

echo '\nGenerate Figure A6.\nOutput stored in data/summResults/netPerfCoef_ameSR.pdf.'

Rscript runResults/amenModelCompare/netPerfSecondary_amen.R 2>&1 >/dev/null 

echo 'Replication complete, check data/summResults directory for outputs.\n'