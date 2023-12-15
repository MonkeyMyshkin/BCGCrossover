#assess goodness of fit of fitted ERGM

ergm.fit.gof<-gof(ergm.fit)

#save summary of goodness of fit of fitted ERGM for region reg
png(paste('./transmission_model/figures/gof_ERGM_fit_', reg, '.png', sep=""), width = 800, height = 960, pointsize=20)
par(mfrow=c(3,2))
plot(ergm.fit.gof, main = ergm.fit$call[2])
par(mfrow=c(1,1))
dev.off()


