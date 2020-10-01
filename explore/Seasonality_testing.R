df_GLRI <- readRDS(file.path("process","out","glri_summary.rds"))
df <- df_GLRI
df$sHM <- df$BACHUM.cn.100mls + df$Lachno.2.cn.100ml

plot(df$BACHUM.cn.100mls + df$Lachno.2.cn.100ml~as.POSIXlt(df$psdate)$mon,log="y")
plot(df$F~as.POSIXlt(df$psdate)$mon,log="y")

df$month <- as.POSIXlt(df$psdate)$mon
winter <- filter(df,month %in% c(11,0,1))
summer <- filter(df,month %in% c(5:8))

boxplot(df$BACHUM.cn.100mls + df$Lachno.2.cn.100ml~as.POSIXlt(df$psdate)$mon,log="y")
boxplot(df$F~as.POSIXlt(df$psdate)$mon,log="y")

plot(df$sHM ~ df$T,col = df$month,log = "y")

wilcox.test(winter$Lachno.2.cn.100ml, summer$Lachno.2.cn.100ml)
wilcox.test(winter$BACHUM.cn.100mls, summer$BACHUM.cn.100mls)

wilcox.test(winter$ent , summer$ent)
wilcox.test(winter$eColi , summer$ent)
wilcox.test(winter$ , summer$ent)

wilcox.test(winter$bacHum, summer$bacHum)
wilcox.test(winter$bacHum, summer$bacHum)
wilcox.test(winter$bacHum, summer$bacHum)
