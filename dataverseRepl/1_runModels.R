#set up workspace ################### 
rm(list=ls())

seed <- 12345
set.seed(seed)
mainPath = '~/Research/netModels/dataverseRepl/'

# load libraries
pkgs = c(
    'network','sna','ergm','latentnet','btergm','amen',
    'reshape2','plyr','ggplot2','latex2exp','Cairo',
    'xtable','ROCR','caTools','RColorBrewer','png','grid')
shh=lapply(pkgs, library, character.only=TRUE)

# Set a theme for gg
theme_set(theme_bw())

# misc
char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }

# load data
load(paste0(mainPath, 'data/data.rda'))
####################

#run logit ################### 
if(!file.exists(paste0(mainPath, 'results/logitEst.rda'))){
    model.logit <- glm(
        collab ~ priv.ngo + allopp + prefdist + forum + infrep + 
        influence.icov + influence.absdiff + gov.ifactor + 
        ngo.ofactor + type.nodematch + collab.t, 
        data = logit.data, family = "binomial")
    save(model.logit, file=paste0(mainPath, 'results/logitEst.rda'))
}
load(paste0(mainPath, 'results/logitEst.rda'))
####################

#run ERGM ################### 
if(!file.exists(paste0(mainPath, 'results/ergmEst.rda'))){
    model.ergm <- ergm(nw.collab ~ 
        edges + 
        edgecov(collab.t) + 
        nodeifactor("orgtype", base = -1) + 
        nodeofactor("orgtype", base = -2) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp) + 
        odegreepopularity + 
        twopath + 
        gwidegree(2, fixed = TRUE) + 
        gwesp(1, fixed = TRUE) + 
        gwodegree(0.5, fixed = TRUE),
        eval.loglik = TRUE, check.degeneracy = TRUE, 
        control = control.ergm(seed = seed, MCMC.samplesize = 5000, 
        MCMC.interval = 5000)
    )
    save(model.ergm, file=paste0(mainPath, 'results/ergmEst.rda'))
}
load(paste0(mainPath, 'results/ergmEst.rda'))
####################

#run LSM ################### 
if(!file.exists(paste0(mainPath, 'results/lsmEst.rda'))){
    set.seed(seed)
    model.ls <- ergmm(nw.collab ~ 
        euclidean(d = 2, G = 0) +  # 2 dimensions and 0 clusters
        edgecov(gov.ifactor) + 
        edgecov(ngo.ofactor) + 
        nodematch("orgtype") + 
        edgecov(priv.ngo) + 
        edgecov(forum) + 
        edgecov(infrep) + 
        nodeicov("influence") + 
        absdiff("influence") + 
        edgecov(prefdist) + 
        edgecov(allopp), 
        seed = seed, 
        control = control.ergmm(sample.size = 10000, burnin = 50000, interval = 100)
    )
    save(model.ls, file=paste0(mainPath, 'results/lsmEst.rda'))
}
load(paste0(mainPath, 'results/lsmEst.rda'))
#################### 

#run amen ################### 
# install amen from s7minhas/github
if(!file.exists(paste0(mainPath, 'results/ameEst.rda'))){
    # mcmc params
    imps = 100000
    brn = 50000
    ods = 10
    latDims = 2

    # Run amen in parallel
    ameFit = ameTest(Y=Y, Xdyad=Xd, Xrow=Xs, Xcol=Xr, 
        model='bin', symmetric=FALSE, R=latDims, 
        nscan=imps, seed=seed, burn=brn, odens=ods, 
        rvar=TRUE, cvar=TRUE,  # row and column effects, standard in srm
        plot=FALSE, print=FALSE) 

    # reduce obj to save
    ameFit = ameFit[c('BETA','GOF')]
    save(ameFit, file=paste0(mainPath, 'results/ameEst.rda'))
}
load(paste0(mainPath, 'results/ameEst.rda'))
#################### 

#build table 1 ################### 
# helpful functions
source(paste0(mainPath, 'coefHelpers.R'))

# org logit results
logitCoef = summary(model.logit)$'coefficients'
logitTab = lazyCleanTable( logitCoef[,c(1:2,4)], sigCol=3 )

# org ergm results
ergmTab = lazyCleanTable( summary(model.ergm)$'coefs'[,c(1:2,4)], sigCol=3 )

# org lsm results
lsEuclCoef = summary(model.ls)$'pmean'$'coef.table'
lsEuclTab = lazyCleanTable( lsEuclCoef, sigCol=4 )

# org ame results
ameCoef = getAmeCoef(ameFit) ; rownames(ameCoef) = gsub('.col','',rownames(ameCoef))
ameTab = lazyCleanTable( ameCoef[,c('pmean','lo95','hi95','p-val')], sigCol=4 )

# summarize in q & d table
coefDfs = list(Logit=logitTab, LSM=lsEuclTab, ERGM=ergmTab, 'AME'=ameTab)

# table
frameRows = rep(varKey[,2], each=2)
frameRows = append(frameRows, '\\textbf{Conflicting policy preferences}', which(frameRows=='Intercept/Edges')[2])
frameRows = append(frameRows, '\\textbf{Transaction costs}', which(frameRows=='Preference dissimilarity')[2])
frameRows = append(frameRows, '\\textbf{Influence}', which(frameRows=='Joint forum participation')[2])
frameRows = append(frameRows, '\\textbf{Functional requirements}', which(frameRows=='Alter = Government actor')[2])
frameRows = append(frameRows, '\\textbf{Endogenous dependencies}', which(frameRows=='Same actor type')[2])
frame = matrix('', nrow=length(frameRows), ncol=length(coefDfs)+1, 
    dimnames=list( frameRows, c(' ', names(coefDfs)) ))

# helpful ids
varPos = which(frameRows %in% varKey[,2])
estRows = varPos[seq(1,length(varPos),2)]; errorRows = varPos[seq(2,length(varPos),2)]
varLab = frameRows; varLab[errorRows] = ''; frame[,1] = varLab; rownames(frame)[estRows] = ''

# Add logit results
frame = insertCoefInfo(frame, model='Logit')
frame = insertCoefInfo(frame, model='LSM', error='int')
frame = insertCoefInfo(frame, model='ERGM')
frame = insertCoefInfo(frame, model='AME', error='int')

# cleanup
rownames(frame) = NULL
frame[estRows[-1],1] = paste0('$\\;\\;\\;\\;$ ', frame[estRows[-1],1])

# print
print.xtable(
    xtable(frame, align='llcccc',
        caption='* p $<$ 0.05 (or 0 outside the 95\\% confidence interval).',
        label='tab:regTable'), 
    include.rownames=FALSE,
    sanitize.text.function=identity,
    hline.after=c(0,0,nrow(frame),nrow(frame)),
    size='normalsize', file=paste0(mainPath, 'floats/Table3.tex')
)
#################### 

#build figure 3 ################### 
# helpful functions
source(paste0(mainPath, 'netPerfHelpers.R'))

# actual values
actVals = gofstats(Y)
perfNetKey = cbind(v=names(actVals), 
    c=c('Sender variation', 
        'Receiver variation', 
        'Dyadic dependency', 
        'Triadic dependency') )

# ERGM
ergmSims = simulate.ergm(model.ergm,nsim=1000)
ergmPerf = do.call('rbind', lapply(ergmSims, function(x){
    gofstats( as.sociomatrix( x ) ) }))

# AME
amePerf = ameFit$'GOF'[-1,]

# LS - EUCL
# lsEuclSim = simulate.ergmm(model.ls, nsim=100)
lsEuclSim = simulate.ergmm(model.ls, nsim=100)
lsEuclPerf = do.call('rbind', lapply(lsEuclSim$'networks', function(x){ 
    gofstats( as.sociomatrix( x ) ) }))

# org
perfList = list(AME=amePerf, ERGM=ergmPerf, LSM=lsEuclPerf)

# viz
getNetPerfCoef(
    perfList, perfNetKey, actVals, 
    pRows=1, save=TRUE, 
    fPath=paste0(mainPath, 'floats/Figure3_color.pdf'))

fig3=getNetPerfCoef(
    perfList, perfNetKey, actVals, 
    pRows=1, save=FALSE) +
    theme_bw() + 
    scale_color_manual(values=rep('black',3)) +
    theme(
        legend.position='none', legend.title=element_blank(),
        legend.key=element_rect(color=NA),
        axis.ticks=element_blank(), panel.border=element_blank()
        )    
ggsave(fig3, width=9, height=4,
    file=paste0(mainPath, 'floats/Figure3_bw.pdf'))
#################### 