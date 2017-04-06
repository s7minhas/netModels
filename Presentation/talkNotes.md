MPSA '17 Soc Infl Talk
---

# Slide 1: Motivation #

Relational data are composed of observations indicating whether a pair of actors share a link. These links can take the shape of simple undirected, binary relations observed for a snapshot in time to more complex types of directed and weighted relations that are observed longitudinally.

The study of these types of data is made more interesting by the possibility that the relations observed do not arise or evolve independent of one another. Observations in relational data may be simultaneously dependent on all other observations due to the social ties and pathways that give shape to the global structure in which actors are embedded. This dependence is why many study relational data not as a set of independent dyadic observations, but as a network in which the link between a pair of actors influences and is influenced by other dyads.

# Slide 2: Studying dependencies #

the two sets of statistical approaches that continue to receive the most attention are exponential random graph models (ERGMs) and latent variable models. For ERGMs a set of network statistics (e.g., mixed two-paths, balanced triangles) are specified to characterize the dependencies in the observed network . While latent variable approaches model broader patterns, such as homophily and stochastic equivalence, as a function of node-specific latent variables.

Though ERGM and latent variable approaches offer markedly different ways to model networks, each focuses on explaining the ways in which observations are interdependent based on endogenous characteristics of the network. Thus these extant approaches are effective at characterizing influence patterns that emerge in the network, but they are only able to explain those patterns through endogenous explanations.

# Slide 3: latent variable model #

For example, actors that cluster together in the Euclidean space estimated from latent distance models or that are assigned to similar blocks by latent class models are assumed to possess some set of similar characteristics based on dependence patterns in the network. Yet, these approaches leave the question of what those characteristics are unanswered.

# Slide 4: bilinear autoregression #

To address this question, we build on the bilinear network autoregression model introduced by \citet{hoff:2015}. 2015}. At its core, this approach is a vector autoregression model extended to handle relational data. Within this approach dependencies between observations are captured by a pair of $n \times n$ matrices that measure sender and receiver level influence patterns.

# Slide 5: still inadequate #

The way that we conceptualized influence here was again somewhat similar to the latent variable model approach. Specifically, ....

# Slide 6: Social Influence Regression #

Our approach to extending the bilinear autoregression model to explain patterns of influence involves redefining $A$ and $B$ as a function of a set of influence covariates.

Particularly, to determine the characteristics of $i$ or $i'$ that are related to the influence $a_{ii'}$, we consider a linear regression model for $a_{ii'}$ and $b_{jj'}$, given by $a_{ii'} = \alpha^T w_{ii'}$ and $b_{jj'} = \beta^T w_{jj'}$, where $w_{ii'}$ is a vector of nodal and dyadic covariates specific to pair $ii'$ that we are using to estimate influence. In the application we present in the following section, we have time-varying covariates, which this model is able to account for through time varying influence parameters: $a_{ii't} = \alpha^T w_{ii't}$ and $b_{jj't} = \beta^T w_{jj't}$.

# Slide 7: Summary #

Typically, we also want to predict $y_{i,j,t}$ with additional variables. For example, we might want to condition estimation of the parameters on a lagged version of the dependent variable, $y_{i,j,t-1}$, a measure of reciprocity, $y_{j,i,t-1}$, and other exogenous variables. In the case of estimating a model on material conflict between a pair of countries, this would obviously include other exogenous parameters such as the distance between a pair of countries. Additional parameters such as these can be accommodated with a model of the form:

where $z_{i,j,t}$ represents the design array in which we incorporate parameters that may have a direct effect on the dependent variable.

The model presented here is a type of low-rank matrix regression: We are regressing the outcome $y_{ij,t}$ on the matrix $X_{ij,t}$.

To estimate the parameters, $\{\theta, \alpha, \beta \}$, we employ an iterative process since the model is bilinear.

Specifically, for a fixed $\beta$ the model is linear in $(\theta, \alpha)$, and for fixed $\alpha$ the model is linear in $(\theta, \beta)$

One way to approach this problem is through an iterative block coordinate descent method for estimation of $\theta$, $\alpha$ and $\beta$. Given initial values of $\beta$, iterate the following until convergence:

\begin{enumerate}
  \item Find the conditional maximum likelihood estimate (MLE) of $(\theta,\alpha)$ given $\beta$ using iterative weighted least squares (IWLS);
  \item Find the conditional MLE of $(\theta,\beta)$ given $\alpha$ using IWLS.
\end{enumerate}

Using this approach the problem of finding the conditional MLEs turns into a sequence of low dimensional generalized linear model (GLM) optimizations.

\begin{enumerate}[(a)]
  \item Let $\tilde x_{ij,t}$ be the vector of length $p+q$ obtained by concatenating $z_{ij,t}$  and  $X_{ij,t}\beta$.
  \item Construct the matrix $\tilde X$ having $n\times (n-1)\times T$ rows and $p+q$ columns, where each row is equal to $\tilde x_{ij,t}$ for some (directed) pair $i,j$ at time $t$.
  \item Let $y$ be the vector of length $n\times (n-1)\times T$ consisting of the entries of $Y=\{ Y_1,\ldots, Y_T\}$, ordered to correspond to the rows of $\tilde X$.
  \item Obtain the MLEs for the Poisson regression of $y$ on $\tilde X$. From the regression coefficients, extract the (conditional) estimates of $\theta$ and $\alpha$.
\end{enumerate}

# Slide 8: Application #

Over the past few years, a number of projects have arisen seeking to create large data sets of dyadic events through the automatic extraction of information from on-line news archives.

The two most well known developments include the ICEWS event data project \citep{icews:2015:data} and the Phoenix pipeline \citep{oeda:2016}. For the purposes of this project we focus on utilizing the ICEWS database since it extends back farther in time. ICEWS draws from over 300 different international and national focused publishers \citep{boschee:etal:2015}. The ICEWS data along with extensive documentation have been made publicly available on \url{dataverse.org} \citep{icews:2015:aggregations,icews:2015:data}. To classify news stories into socio-political topics, ICEWS relies on the CAMEO coding scheme \citep{schrodt:etal:2009}. This scheme parses news stories for dyadic events and, specifically, searches for the following information: a sender, a receiver, an action type, and a time stamp.

Our sample for this analysis focuses on monthly level interactions between countries in the international system from 2005 to 2012.\footnote{The ICEWS data extends to 2016 but we end at 2012 due to temporal coverage constraints among other covariates that we have incorporated into the model.} To measure conflict from this database we focus on what is often referred to as the ``material conflict'' variable. This variable is taken from the ``quad variable'' framework developed by \citet{duval:thompson:1980}. \citet{schrodt:yonamine:2013} defines the type of events that get drawn into this category as those involving, ``Physical acts of a conflictual nature, including armed attacks, destruction of property, assassination, etc.''.

visualizes the material conflict variable as a network

# Slide 9: Model Specification #

\begin{itemize}
  \item what parameters can explain why the actions of the USA at time $t-1$ are predictive of the actions of the GBR at time $t$ and
  \item why are actions directed at the USA at time $t-1$ predictive of the actions directed towards the GBR at time $t$.
\end{itemize}

# Slide 10: Model Results #

# Slide 11/12: Dependence patterns #

# Slide 13: Cross-validation #

# Slide 14: Forecasting #

# Slide 15:  #

Thanks