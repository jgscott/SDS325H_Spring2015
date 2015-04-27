library(mosaic)
library(lme4)


# Benjamini-Hochberg with two-sided p-values
BenjaminiHochberg = function(zscores, fdr_level) {
  # zscores is a vector of z scores
  # fdr_level is the desired level (e.g. 0.1) of control over FDR
  # returns a binary vector where 0=nofinding, 1=finding at given FDR level
  N = length(zscores)
  pval2 = 2*pmin(pnorm(zscores), 1- pnorm(zscores))
  cuts = (1:N)*fdr_level/N
  bhdiff = sort(pval2)-cuts
  bhcutind2 = max(which(bhdiff < 0))
  bhcut2 = sort(pval2)[bhcutind2]
  0+{pval2 <= bhcut2}
}


hist(hiv$z, breaks=100, prob=TRUE, col='grey', border=NA)
fdr1 = BenjaminiHochberg(hiv$z, 0.1)
which(fdr1==1)

sort(abs(hiv$z[which(fdr1==1)]))
rug(hiv$z[which(fdr1==1)])

# Compare with normal distribution
hist(rnorm(7680, 0, 1), breaks=100, prob=TRUE, col='grey', border=NA)
