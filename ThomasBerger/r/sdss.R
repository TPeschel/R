sds <- function(value, age, sex, item, ref.obj, male = "male", female = "female"){
    ref <- slot(ref.obj,"ref")[[item]]
    sex <- as.character(sex)
    sapply(1:length(value),function(i){
        if(is.na(value[i] | is.na(age[i]) | is.na(sex[i]))) return(NA)
        m <- approx(ref[[sex[i]]]$age,ref[[sex[i]]]$m,xout=age[i],rule=1)$y
        l <- approx(ref[[sex[i]]]$age,ref[[sex[i]]]$l,xout=age[i],rule=1)$y
        s <- approx(ref[[sex[i]]]$age,ref[[sex[i]]]$s,xout=age[i],rule=1)$y
        ((value[i]/m)**l-1)/(l*s)
        })
}



## fuer BCCG
sds <- function(age,val,sex){ 
    l <- approx(ref$age,ref[,paste(sex,"nu",sep = ".")],xout=age,rule=2)$y
    m <- approx(ref$age,ref[,paste(sex,"mu",sep = ".")],xout=age,rule=2)$y
    s <- approx(ref$age,ref[,paste(sex,"sigma",sep = ".")],xout=age,rule=2)$y
    ((val/m)**l-1)/(l*s)
  }
  
  
sds <- function(age,val,sex){ 
    l <- approx(ref$age,ref[,paste(sex,"nu",sep = ".")],xout=age,rule=2)$y
    m <- approx(ref$age,ref[,paste(sex,"mu",sep = ".")],xout=age,rule=2)$y
    s <- approx(ref$age,ref[,paste(sex,"sigma",sep = ".")],xout=age,rule=2)$y
    tau <- approx(ref$age,ref[,paste(sex,"tau",sep = ".")],xout=age,rule=2)$y
    pBCPEo(val, mu = m, sigma = s, nu = l, tau = tau)
}
  
  
sdsdf <- function(x,alter,sex,value,type="form"){
    if(is.na(x[value])){return(NA)}
    sds(age=as.numeric(x[alter]),sex=x[sex],val=as.numeric(x[value]))
}


data$sdsval <- apply(data,1,sdsdf,alter = "alter", sex = "sex", value = "value")

names(res.girls) <- 1:length(res.girls)
names(res.boys) <- 1:length(res.boys)

res.girls <- lapply(res.girls,function(x) {
    x$perc3 <- calc.vals(perc=0.03,x$mu,x$sigma,x$nu)
    x$perc10 <- calc.vals(perc=0.10,x$mu,x$sigma,x$nu)
    x$perc50 <- calc.vals(perc=0.5,x$mu,x$sigma,x$nu)
    x$perc90 <- calc.vals(perc=0.9,x$mu,x$sigma,x$nu)
    x$perc97 <- calc.vals(perc=0.97,x$mu,x$sigma,x$nu)
    return(x)
})

res.boys <- lapply(res.boys,function(x) {
    x$perc3 <- calc.vals(perc=0.03,x$mu,x$sigma,x$nu)
    x$perc10 <- calc.vals(perc=0.10,x$mu,x$sigma,x$nu)
    x$perc50 <- calc.vals(perc=0.5,x$mu,x$sigma,x$nu)
    x$perc90 <- calc.vals(perc=0.9,x$mu,x$sigma,x$nu)
    x$perc97 <- calc.vals(perc=0.97,x$mu,x$sigma,x$nu)
    return(x)
})

n.iter <- length(res.girls)
print(n.iter)
res.girls <- Reduce(rbind,res.girls)
res.girls$which <- rep(1:n.iter,each = 214)

n.iter <- length(res.boys)
print(n.iter)
res.boys <- Reduce(rbind,res.boys)
res.boys$which <- rep(1:n.iter,each = 214)

perc.single.girls <- melt(res.girls,id.vars = c("which","age","mu","sigma","nu","tau"))
perc.single.boys <- melt(res.boys,id.vars = c("which","age","mu","sigma","nu","tau"))

perc.sum.girls <- res.girls %>% group_by(age) %>%
    summarise(
        mean.mu = mean(mu),
        sd.mu = sd(mu),
        mean.sigma = mean(sigma),
        sd.sigma = sd(sigma),
        mean.nu = mean(nu),
        sd.nu = sd(nu),
        mean.tau = mean(tau),
        sd.tau = sd(tau)
    )


perc.sum.boys <- res.boys %>% group_by(age) %>%
    summarise(
        mean.mu = mean(mu),
        sd.mu = sd(mu),
        mean.sigma = mean(sigma),
        sd.sigma = sd(sigma),
        mean.nu = mean(nu),
        sd.nu = sd(nu),
        mean.tau = mean(tau),
        sd.tau = sd(tau)
    )

perc.sum.girls$mean.perc3  <- calc.vals(perc=0.03,perc.sum.girls$mean.mu,perc.sum.girls$mean.sigma,perc.sum.girls$mean.nu)
perc.sum.girls$mean.perc10 <- calc.vals(perc=0.10,perc.sum.girls$mean.mu,perc.sum.girls$mean.sigma,perc.sum.girls$mean.nu)
perc.sum.girls$mean.perc50 <- calc.vals(perc=0.50,perc.sum.girls$mean.mu,perc.sum.girls$mean.sigma,perc.sum.girls$mean.nu)
perc.sum.girls$mean.perc90 <- calc.vals(perc=0.90,perc.sum.girls$mean.mu,perc.sum.girls$mean.sigma,perc.sum.girls$mean.nu)
perc.sum.girls$mean.perc97 <- calc.vals(perc=0.97,perc.sum.girls$mean.mu,perc.sum.girls$mean.sigma,perc.sum.girls$mean.nu)

perc.sum.boys$mean.perc3  <- calc.vals(perc=0.03,perc.sum.boys$mean.mu,perc.sum.boys$mean.sigma,perc.sum.boys$mean.nu)
perc.sum.boys$mean.perc10 <- calc.vals(perc=0.10,perc.sum.boys$mean.mu,perc.sum.boys$mean.sigma,perc.sum.boys$mean.nu)
perc.sum.boys$mean.perc50 <- calc.vals(perc=0.50,perc.sum.boys$mean.mu,perc.sum.boys$mean.sigma,perc.sum.boys$mean.nu)
perc.sum.boys$mean.perc90 <- calc.vals(perc=0.90,perc.sum.boys$mean.mu,perc.sum.boys$mean.sigma,perc.sum.boys$mean.nu)
perc.sum.boys$mean.perc97 <- calc.vals(perc=0.97,perc.sum.boys$mean.mu,perc.sum.boys$mean.sigma,perc.sum.boys$mean.nu)


perc.sum.girls <- melt(perc.sum.girls,id.vars = c("age",
                                                  "mean.mu","mean.sigma","mean.nu","mean.tau",
                                                  "sd.mu","sd.sigma","sd.nu","sd.tau"))
perc.sum.boys <- melt(perc.sum.boys,id.vars = c("age",
                                                "mean.mu","mean.sigma","mean.nu","mean.tau",
                                                "sd.mu","sd.sigma","sd.nu","sd.tau"))

p1 <- ggplot(perc.single.girls,aes(x=age,
                                   y=value,
                                   colour=variable,
                                   group=paste(variable,which))) +
    geom_line(alpha=0.01) +
    ##            scale_colour_manual(values = c("firebrick","orangered","forestgreen","orangered","firebrick")) +
    scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
    geom_line(data = perc.sum.girls, inherit.aes = F,
              aes(x=age,y=value,group=variable),
              colour = "black") +
    ##                    xlim(c(2.5,16)) +
    ##                        ylim(limits[[mg]]) +
    theme_bw() +
    theme(
        legend.position = "none"
    )
ggsave(paste0(mg,"girlsgrey.png"),plot=p1, width = 29, height = 21, units = "cm")

p2 <- ggplot(perc.single.boys,aes(x=age,
                                  y=value,
                                  colour=variable,
                                  group=paste(variable,which))) +
    geom_line(alpha=0.01) +
    ##            scale_colour_manual(values = c("firebrick","orangered","forestgreen","orangered","firebrick")) +
    scale_colour_manual(values = c("grey","grey","grey","grey","grey")) +
    geom_line(data = perc.sum.boys, inherit.aes = F,
              aes(x=age,y=value,group=variable),
              colour = "black") +
    ##                    xlim(c(2.5,16)) +
    ##                    ylim(limits[[mg]]) +
    theme_bw() +
    theme(
        legend.position = "none"
    )
