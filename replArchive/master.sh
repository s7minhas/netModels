#!/bin/sh
{
echo '\nStarting replication.
	All output will be stored in data/summResults directory.
	Table and Figure numbers from paper are referenced when needed.
		Deleting log file and existing files in output directories.
		Recreating output directories if necessary.
	'
rm -rf masterLog
rm -rf data/summResults/*.png
rm -rf data/summResults/*.tex
rm -rf data/summResults/*.pdf
rm -rf data/rdaResults/*.rda
mkdir -p data/summResults/
mkdir -p data/rdaResults/

echo '\n-------------
Build data files.
	Output files will be stored in data/data/*.rda.
...working'
Rscript replicationSetup.R >/dev/null
echo '---------done'

echo '\n-------------
Generate Figure 2.
	Output will be stored in data/summResults/:
			dvNet_outDegree.pdf
			dvNet_inDegree.pdf.
...working'
Rscript other/dvNet.R >/dev/null
echo '---------done'

echo '\n-------------
Running models (ERGM, LSM, LSM (Bilinear), Logit, MRQAP, AMEN).
	This takes about one hour.
	Output for various model types will be stored in data/rdaResults
	and diagnostic plots stored in data/summResults.
...working'	
Rscript runMods/runERGM.R &>/dev/null ; echo '\t\tERGM results computed.'
Rscript runMods/runLatentSpace.R &>/dev/null ; echo '\t\tLSM results computed.'
Rscript runMods/runLatentSpaceBil.R &>/dev/null ; echo '\t\tLSM Bilinear results computed.'
Rscript runMods/runLogit.R &>/dev/null ; echo '\t\tLogit results computed.'
Rscript runMods/runQAP.R &>/dev/null ; echo '\t\tMRQAP results computed.'
Rscript runMods/runAMEN.R &>/dev/null ; echo '\t\tAMEN results computed.
---------done'

echo '\n-------------
Generate Table 4.
	Output will be stored in data/summResults/regTable.tex.
...working'
Rscript runResults/coefCompare.R &>/dev/null
echo '---------done'

echo '\n-------------
Generate Figure 3.
	Output will be stored in data/summResults/ameFitSR_2_UV.pdf.
...working'
Rscript runResults/latSpaceViz.R &>/dev/null
echo '---------done'

echo '\n-------------
Run out-of-sample performance analysis.
	This will take ~12 hours and will monopolize eight cores on your machine.
	If desired, reset clusters to utilize on line 10 of outPerf/runMods_wMiss.R.
	Output for various model types will be stored in data/rdaResults.
...working'
Rscript outPerf/runMods_wMiss.R &>/dev/null
echo '---------done'

echo '\n-------------
Generate Figure 4.
	Output will be stored in data/summResults/:
		roc_outSample.pdf
		rocPr_outSample.pdf
		Various separation plots corresponding to model type
		aucTable_outSample.tex.
...working'
Rscript outPerf/outSampPerf.R &>/dev/null
echo '---------done'

echo '\n-------------
Generate Figure 5.
	Output will be stored in data/summResults/netPerfCoef.pdf.
...working'
Rscript runResults/netPerfSecondary.R &>/dev/null 
echo '---------done'

echo '\nPaper Table/Figure replication complete.\nStarting on appendix.\n'

echo '\n-------------
Generate Figure A1.
	Output will be stored in data/summResults/:
		ameConv2_SR2.pdf
		ameConv2_SR2.pdf.
...working'
Rscript runResults/amenModelCompare/checkConv_amen.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Figure A2.
	Output will be stored in data/summResults/ggGofAll.pdf.
...working'
Rscript runResults/netPerf.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Table A2.
	Output will be stored in data/summResults/regTable_latSpace.tex.
...working'
Rscript runResults/latSpaceModelCompare/coefCompare_latSpace.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Figure A3.
	Output will be stored in data/summResults/:
		roc_latSpace_outSample.pdf
		rocPr_latSpace_outSample.pdf
		Various separation plots corresponding to model type
		aucTable_latSpace_outSample.tex.
...working'
Rscript outPerf/outSampPerf_latentSpace.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Figure A4.
	Output will be stored in data/summResults/netPerfCoef_latSpace.pdf.
...working'
Rscript runResults/latSpaceModelCompare/netPerfSecondary_latSpace.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Table A3.
	Output will be stored in data/summResults/regTable_ameSR.tex.
...working'
Rscript runResults/amenModelCompare/coefCompare_amen.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Figure A5.
	Output will be stored in data/summResults/:
		roc_ameSR_outSample.pdf
		rocPr_ameSR_outSample.pdf
		Various separation plots corresponding to model type
		aucTable_ameSR_outSample.tex.
...working'
Rscript outPerf/outSampPerf_amen.R &>/dev/null 
echo '---------done'

echo '\n-------------
Generate Figure A6.
	Output will be stored in data/summResults/netPerfCoef_ameSR.pdf.
...working'
Rscript runResults/amenModelCompare/netPerfSecondary_amen.R &>/dev/null 
echo '---------done'

echo '\n\nReplication complete.
	Check data/summResults directory for outputted table and figures.
	Check data/rdaResults for model results.
	Log of script created in masterLog.
	\n'
} | tee masterLog	