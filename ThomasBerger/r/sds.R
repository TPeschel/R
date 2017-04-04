        tr.obj1 <- try(
             mm_boys <- gamlss(
                 value ~ pb(age,2),
                 data = tmpdata_boys,
                 sigma.formula = ~poly(age,2),
                 families = "NO",
                 method.pb = "ML",
                 k = 2,
                 trace = F) )
        
centiles(mm_boys,xvar = tmpdata_boys$age)

tmp <-         as.data.frame(
                 predictAll(
                     mm_boys,
                     newdata = data.frame( age = age )
                 )
             )

tr.obj2 <- try(
             mm_girls <- gamlss(
                 value ~ pb(age,2),
                 data = tmpdata_girls,
                 sigma.formula = ~poly(age,2),
                 families = "NO",
                 method.pb = "ML",
                 k = 2,
                 trace = F) )
  centiles(mm_girls,xvar = tmpdata_girls$age)      
  