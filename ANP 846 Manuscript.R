# Start with a clean working directory by clearing lists
rm(list=ls())
# Remove random
set.seed(1234)

if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
#Install package
#install.packages("OTE")
# load packages
library(OTE) #Contains the dataset "Body" that will be evaluated
library(tidyr) #used in chunks:
library(psych) #used in chunks: c4
library(ggplot2) #used in chunks: c5
library(dplyr)
library(kableExtra) #c11
library(pheatmap) #c11
library(dendextend)#c11
library(mice)
library(naniar)
library(forcats)
library(GGally)
library(gt)
library(viridis)
library(RColorBrewer)
library(plotly)
library(reshape2)
library(gridExtra)
library(descr)
options(descr.plot=FALSE)
library(plyr)
library(ggthemes)
library(stringr)
library(FactoMineR)
library(MASS)
library(ggdendro)
library(ape)
library(corrplot)
library(gtsummary) #c12
library(ggsignif) #c13, may not use
library(ggpubr) #c13, to do sig
library(tibble)#for the corcov table
library(webshot2)#export those gt tables that are being stupid and don't want to be exported for some god forsaken reason
library(patchwork)#for da legends oooo




# Load Data
data <- Body #Source: https://rdrr.io/cran/OTE/man/Body.html
# Rename Data
bdy <- data

#rename gender to match
bdy$Gender <- ifelse(bdy$Gender== 0,"Female","Male")
bdy$Gender

plt1.1 <- ggplot(bdy,aes(Age,fill=Gender))+geom_density(alpha=0.25,linewidth=0.5)+
  theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))+
  labs(y="Density")
#ggsave("agedist.png", plot = plt1.1, width = 8, height = 6, dpi = 300)

bdy$age_grouping <- cut(bdy$Age, seq(10, 70, 10),
                        labels = c("10-20", "21-30", "31-40", "41-50", "51-60","61-70"))
bdy$age1 <- bdy$Age

bdy$Age <- bdy$age_grouping

#bdy$Age <- bdy$age_grouping

###load functions###
flag_outliers <- function(x) {
  q <- quantile(x,probs = c(.25,.75), na.rm=TRUE)
  iqr <- q[2]-q[1]
  upper <- q[2]+1.5*iqr
  lower <- q[2]-1.5*iqr
  x<lower|x>upper
}

pvals <- function(i,j) mapply(function(a,b) cor.test(bdy[[a]], bdy[[b]])$p.value, i, j)

#pvals if sig get stars
stars <- function(p) {
  if (p < .05)  return("*")
  return("")
}




#summary of data to double check that all objects match at this stage
#summary(Body)
#summary(data)
#summary(body)

#look at data briefly
bdy
str(bdy)
describe(bdy)
dim(bdy)
colSums(is.na(bdy))
summary(bdy)
#missing data screening
is.data.frame(bdy)
if(requireNamespace("naniar",quietly=TRUE)){naniar::vis_miss(bdy)}
count(bdy$Gender==1)





ggplot(bdy,aes(Biiliac))+geom_histogram(bins=40)+theme_classic()+labs(x="Billiac measurement in cm", y= "Number of people")
ggplot(bdy,aes(Biacrom))+geom_histogram(bins=30)+theme_classic()+labs(x="Bicrom measurement in cm", y= "Number of people")



bdy$whtr <- bdy$WaistG/bdy$Height


bdy$whtrrisk <- ifelse(bdy$whtr>0.6 & bdy$age1 >50, "High Risk",
                   ifelse(bdy$whtr>0.5 & bdy$age1<51, "High Risk","Low Risk"))

bdy$heightinm <- bdy$Height/100
bdy$bmi <- bdy$Weight/bdy$heightinm^2
bdy$bmirisk <- ifelse(bdy$bmi>=30, "High Risk", "Low Risk")


#risk of both whtr and bmi
athighrisk <- bdy%>%
  filter(whtrrisk =="High Risk", bmirisk == "High Risk")



#bmi risk and not at risk based on whtr
atbmirisk <- bdy%>%
  filter(bmirisk=="High Risk",whtrrisk!= "High Risk")

#at whtr risk but not bmi risk
atwhtrrisk <- bdy%>%
  filter(whtrrisk=="High Risk", bmirisk!= "High Risk" )

athighrisk
atbmirisk
atwhtrrisk



##biiliac

bdy$biiloutlier_by_strata <- ave(bdy$Biiliac,interaction(bdy$whtr,bdy$bmi), FUN= flag_outliers)
table(bdy$biiloutlier_global)
table(bdy$biiloutlier_by_strata)

bdy[bdy$biiloutlier_by_strata,]

whtrmean <- ggplot(bdy, aes(Biiliac,whtr,group=whtrrisk))+
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun=mean,geom="line")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.15)+
  facet_wrap(~whtrrisk)+
  labs( x= "Pelvic Breadth (cm)", y= "Waist to height ratio")+
  theme_classic()
bmimean <- ggplot(bdy, aes(Biiliac,bmi,group=bmirisk))+
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun=mean,geom="line")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.15)+
  facet_wrap(~whtrrisk)+
  labs(x= "Pelvic Breadth (cm)", y= "Body Mass Index")+
  theme_classic()

plotadd <- whtrmean+bmimean+ plot_layout(nrow = 2)

plotadd
#ggsave("ci.png",plotadd)

#biacrom
fit_aovbiacrom <- aov(Biacrom~whtr*bmi, data=bdy)
summary(fit_aovbiacrom)

fit_lmbiacrom <- lm(Biacrom~whtr*bmi, data=bdy)
summary(fit_lmbiacrom)
par(mfrow=c(2,2))
plot(fit_aovbiacrom)
par(mfrow=c(1,1))

#biiliac
fit_aovbiil <- aov(Biiliac~whtr*bmi, data=bdy)
summary(fit_aovbiacrom)

fit_lmbiil <- lm(Biiliac~whtr*bmi, data=bdy)
summary(fit_lmbiil)
par(mfrow=c(2,2))
plot(fit_aovbiil)
par(mfrow=c(1,1))

bdy
which.variables <- c(2,27,31,28)
Breaks <- seq(-1, 1, by=.01)
# select a color palette
my_palette <- c( colorRampPalette( rev(RColorBrewer::brewer.pal(n = 11,
                                                                name ="Spectral")))(length(Breaks)-2) , "grey80", "grey80")

#find pvals
test <- bdy%>%
  dplyr::select(which.variables)
test


mt <- test

mt <- mt%>%
    dplyr::rename(
    `Waist to Height Ratio` = whtr,
    `Body Mass Index` = bmi,
    `Pelvic Bredth`=Biiliac,
    `Age`=age1
  )
mt
#write table
#my.mat <- outer(seq_len(ncol(mt)), seq_len(ncol(mt)), pvals)
#write.csv(my.mat,"pvals.csv")
#my.mat <- cor(bdy[,which.variables],use="p",method="p")
#pvals <- read.csv("pvals.csv")
#pvals

#diag(my.mat) <- apply(bdy[,which.variables],2,sd,na.rm=TRUE)
#my.mat[lower.tri(my.mat)] <- cov(bdy[,which.variables],use="p")[lower.tri(cov(bdy[,which.variables],use="p"))]

#####correlation, covariance, SD####
cor_mat <- cor(mt, use = "pairwise.complete.obs")
cov_mat <- cov(mt, use = "pairwise.complete.obs")
sd_vec  <- apply(mt, 2, sd, na.rm = TRUE)

# p-values for correlations only
get_pval <- function(x, y) cor.test(x, y)$p.value
pval_mat <- outer(
  seq_len(ncol(mt)),
  seq_len(ncol(mt)),
  Vectorize(function(i, j) get_pval(mt[, i],
                                    mt[, j]))
)

# Build combined numerical matrix
combined <- cor_mat
diag(combined) <- sd_vec
combined[lower.tri(combined)] <- cov_mat[lower.tri(cov_mat)]

# Prepare a character version so we can attach stars
combined_char <- matrix(
  as.character(round(combined, 3)),
  nrow = nrow(combined),
  ncol = ncol(combined),
  dimnames = dimnames(combined)
)

# Apply stars ONLY to upper triangle correlations,this piece of shit really wants to go into all of the sections
for (i in 1:ncol(cor_mat)) {
  for (j in 1:ncol(cor_mat)) {

    # Only upper triangle (correlations), like I said ONLY cor
    if (i < j) {
      p <- pval_mat[i, j]
      combined_char[i, j] <- paste0(combined_char[i, j], stars(p))
    }

    # diagonal and lower triangle remain unchanged (Thank god)
  }
}

# Convert to data frame so it can be tableized
trial1.7 <- as.data.frame(combined_char) %>%
  tibble::rownames_to_column("Variable")

trial1.7 <- gt(trial1.7, rowname_col = "Variable") %>%
  # Center column headers
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  # Center all body values
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  # Left-align row labels, i swear this better work I am so sick of the rows being center aligned, yipee it works, now why is there a line there
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
  )%>%
  #Burn the witch (aka the line)
  tab_options(
    stub.border.width = px(0)
  )

#gtsave(trial1.7,"corcovsd.png")
##########################
#write.csv(my.mat,"cor_sd_covary.csv")

heatmapplot <- pheatmap(cor(bdy[,which.variables], use="p", method="p"), cluster_cols=TRUE,
         cluster_rows=TRUE, display_numbers = TRUE,
          breaks=Breaks,
         #labels_row = paste("Wave", 1:7),  labels_col = paste("Wave", 1:7),
         color= my_palette)

#ggsave("heatmapplot.png", plot = heatmapplot, width = 8, height = 6, dpi = 300)

which.vars2 <- c(1:21,23,24,27,28,30,31)
#heat map of all variables
heatmapplot2 <- pheatmap(cor(bdy[which.vars2], use="p", method="p"), cluster_cols=TRUE,
         cluster_rows=TRUE, display_numbers = TRUE,
          breaks=Breaks,
         #labels_row = paste("Wave", 1:7),  labels_col = paste("Wave", 1:7),
         color= my_palette)

#ggsave("heatmapplot2.png", plot = heatmapplot2, width = 8, height = 6, dpi = 300)

#plot cluster
plot(hclust(as.dist(1-cor(bdy[,which.variables],method="p"))), xlab="Distance: 1-correlation")

my.mat <- cor(bdy[,which.variables],use="p",method="s")
#my.mat <- ifelse(my.mat < 0.05, paste0("**", sprintf("%.2f", my.mat), "**"),sprintf("%.2f", my.mat))

diag(my.mat) <- apply(bdy[,which.variables],2,sd,na.rm=TRUE)
my.mat[lower.tri(my.mat)] <- cov(bdy[,which.variables],use="p")[lower.tri(cov(bdy[,which.variables],use="p"))]
write.csv(my.mat,"cor_sd_covary.csv")


#my.mat <- read.csv("cor_sd_covary.csv")
kable(my.mat, escape = FALSE) %>%
  kable_styling(full_width = FALSE)
kable(my.mat,digits=22,caption="Correlation/SD/coariances between variables") %>% kable_styling()



#distances of individuals based on complete observations and how they relate to each other
pheatmap(bdy[,which.variables],scale="row",color=my_palette,show_rownames = TRUE)
clust.compl <- hclust(dist(bdy[,which.variables]),method="complete")
plot(clust.compl)

table(cutree(clust.compl,k=5))
nodePar <- list(lab.cex=0.6,pch=c(NA,5), cex=0.7,col=my_palette)


dend <- as.dendrogram(hclust(dist(bdy[,which.variables]),method = "complete"),
                       nodePar=nodePar,leaflab="none")
dend1 <- color_branches(dend,k=5)
plot(dend1,type="rectangle",nodePar=nodePar,leaflab="none", ylab="Height",xlab="Individuals")
par(mfrow=c(1,1))
par(mar=c(12,4,1,1))

#creating a missing variable table, no missing variables discard tableage1

min(bdy$age1)
max(bdy$age1)



tab1 <- bdy  %>%
  dplyr::select(Biacrom,Biiliac,Bitro,ChestDp,ChestD,ElbowD,WristD,KneeD,AnkleD,ShoulderG,ChestG,WaistG,AbdG,HipG,ThighG,BicepG,ForearmG,KneeG,CalfG,AnkleG,Age,Weight,Height,Gender
  ) %>%    miss_var_summary() %>%
  gt::gt() %>%
  gt::cols_label(
    variable = "Variable",
    n_miss = "Missing (count)",
    pct_miss = "Missing (%)"
  ) %>%
  gt::fmt_number(
    columns = c(pct_miss),
    decimals = 2
  )
tab1
###



tab2 <- bdy%>%
  dplyr::select(Age,Gender)%>%
  tbl_summary()

tab2
gt2 <- as_gt(tab2)
#gtsave(gt2,"age_group_by_gender.png")

###
tab2.1 <- bdy%>%
  dplyr::select(Age,Gender)%>%
  tbl_cross()

tab2.1
gt2.1 <- as_gt(tab2.1)
#gtsave(gt2.1,"age_group_and_gender.png")


  
  
####  
tab3 <- bdy%>%
  dplyr::select(age_grouping,whtrrisk,Gender)%>%
  mutate(Age = paste(age_grouping))%>%
   mutate(whtrrisk = paste(whtrrisk)) %>%
  tbl_strata(strata=Gender,
             .tbl_fun = ~.x %>%
               tbl_summary(by=whtrrisk,include = age_grouping,label = list(age_grouping ~ "Age Group"))%>%
               add_n(),
             .header = "**{strata}**",N={n}
            )
tab3
gt31 <- as_gt(tab3)
#gtsave(gt31,"WTHRRISK.png")

###
tab4 <- bdy%>%
  dplyr::select(Age,bmirisk,Gender)%>%
  
   mutate(bmirisk = paste(bmirisk)) %>%
  tbl_strata(strata=Gender,
             .tbl_fun = ~.x %>%
               tbl_summary(by=bmirisk)%>%
               add_n(),
             .header = "**{strata}**",N={n}
            )
tab4
gt4 <- as_gt(tab4)
gt4
#gtsave(gt4,"BMIRISK.png")




billi1 <- ggplot(bdy,aes(whtrrisk,Biiliac,fill=Gender))+
  geom_boxplot()+
  geom_signif(comparisons = list(c("Low Risk","High Risk")),annotation = "*")+
  theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))

billi1.1 <- billi1+
  labs(x="Risk Establshed by Weight to Height Ratio",
       y= "Pelvic Bredth (cm)",
       fill="Gender")+
  theme(legend.position="bottom")

billi1.1
#ggsave("billi1.1.png", plot = billi1.1, width = 8, height = 6, dpi = 300)

#####ttesting#####
billit <- ggplot(bdy,aes(whtrrisk,Biiliac,fill=Gender))+
  geom_boxplot()+
  geom_signif(test="t.test",comparisons = list(c("Low Risk","High Risk")),annotation = "*")+
  theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))

billit.1 <- billit+
  labs(x="Risk Establshed by Weight to Height Ratio",
       y= "Pelvic Bredth (cm)",
       fill="Gender")+
  theme(legend.position="bottom")

billit.1
billit.1
#ggsave("billi1.1.png", plot = billi1.1, width = 8, height = 6, dpi = 300)

t.test(data=bdy,Biiliac~whtrrisk)

# whats next?
#####LDA#####

fit.18 <- lda(whtrrisk~Biiliac,data=bdy)
pred1 <- predict(fit.18)
pred.class <- pred1$class
pred.class
table(pred1$class,bdy$whtrrisk)
bdy2 <- bdy
bdy2$lda_class <- pred1$class
bdy2$LD1 <- pred1$x[,1] 
bdy2$pred_class <- pred1$class

bdy2 <- bdy2 %>%
  mutate(misclassified = ifelse(whtrrisk != pred_class, "Yes", "No"))

ldamistakes <- ggplot(bdy2, aes(misclassified,fill=Gender))+
  geom_bar(position="dodge")+theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))+
  labs(y="Count",x="Missclassified")+facet_wrap(vars(age_grouping))
  
#ggsave("ldamist.png", plot = ldamistakes, width = 8, height = 6, dpi = 300)

#get the formula
coef <- fit.18$scaling

coef
cat("LD1 =",coef,"* Pelvic Bredth in cm")

##find percent accurate

actual_class <- factor(bdy$whtrrisk, levels = levels(pred.class))
actual_class 
cm <- table(Predicted = pred.class, Actual = actual_class)
cm

accuracy <- sum(diag(cm)) / sum(cm) * 100
accuracy





#search for packages. This is for the Rmd version so it has been commented out


#search_package_usage <- function(file, pkg, ignore_comments = TRUE) {
#  # Read Rmd
#  rmd_lines <- readLines(file)
  
#  # Identify code chunks
#  start <- grep("^```\\{r", rmd_lines)
#  end   <- grep("^```$", rmd_lines)
#  if(length(start) == 0) stop("No R code chunks found")
  
#  code_lines <- character()
#  code_line_numbers <- integer()
  
#  for(i in seq_along(start)) {
#    s <- start[i]
#    e <- end[end > s][1]
#    chunk <- rmd_lines[(s+1):(e-1)]
    
#    if(ignore_comments) {
#      non_comment_idx <- which(!grepl("^\\s*#", chunk))
#      chunk <- chunk[non_comment_idx]
#      lines_nums <- (s+1):(e-1)
#      lines_nums <- lines_nums[non_comment_idx]
#    } else {
#      lines_nums <- (s+1):(e-1)
#    }
    
#    code_lines <- c(code_lines, chunk)
#    code_line_numbers <- c(code_line_numbers, lines_nums)
#  }
  
#  # Package functions
#  if(!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package ", pkg, " not installed"))
#  pkg_funs <- ls(paste0("package:", pkg))
#  pattern <- paste0("\\b(", pkg, "::)?(", paste(pkg_funs, collapse="|"), ")\\b")
  
#  # Search each line and expand multiple matches
#  results <- data.frame(line_in_rmd = integer(),
#                        matched_text = character(),
#                        full_line = character(),
#                        stringsAsFactors = FALSE)
  
#  for(i in seq_along(code_lines)) {
#    matches <- str_extract_all(code_lines[i], pattern)[[1]]
#    if(length(matches) > 0) {
#      results <- rbind(
#        results,
#        data.frame(
#          line_in_rmd = rep(code_line_numbers[i], length(matches)),
#          matched_text = matches,
#          full_line = rep(code_lines[i], length(matches)),
#          stringsAsFactors = FALSE
#        )
#      )
#    }
#  }
  
#  results
#}

##do not put "ape" in, it will crash
#pkgs <- c(
#  "OTE", "tidyr", "psych", "ggplot2", "dplyr", "kableExtra", "pheatmap",
#  "dendextend", "mice", "naniar", "forcats", "GGally", "gt", "viridis",
#  "RColorBrewer", "plotly", "reshape2", "gridExtra", "descr", "plyr",
#  "ggthemes", "stringr", "FactoMineR", "MASS", "ggdendro", "corrplot",
#  "gtsummary", "ggsignif", "ggpubr", "tibble", "webshot2", "patchwork"
#)

#all_usage <- data.frame(package = character(),
#                        matched_text = character(),
#                        stringsAsFactors = FALSE)

## Loop through packages
#for(pkg in pkgs) {
#  usage <- search_package_usage("ANP 846 Manuscript.Rmd", pkg)
  
#  if(nrow(usage) > 0) {
#    usage$package <- pkg
#    usage <- usage[, c("package", "matched_text")]
#    all_usage <- bind_rows(all_usage, usage)
#  }
#}
#if(nrow(all_usage) == 0) {
#  stop("No package functions found in the Rmd file.")
#}

#unique_usage <- all_usage %>%
#  distinct(package, matched_text)
#unique_usage

#package_summary <- aggregate(
#  matched_text ~ package,
#  data = unique_usage,
#  FUN = function(x) paste(x, collapse = ", ")
#)
#package_summary <- package_summary[order(package_summary$package), ]

#package_summary

#unique_usage <- all_usage %>%
#  distinct(package, matched_text)


#summary_table <- package_summary %>%
#  gt() %>%
#  tab_header(
#    title = "Functions Used per Package",
#    subtitle = "Extracted from R Markdown"
#  ) %>%
#  cols_label(
#    package = "Package",
#    matched_text = "Functions"
#  ) %>%
#  tab_options(
#    table.width = pct(100),
#    heading.title.font.size = 16,
#    heading.subtitle.font.size = 12,
#    data_row.padding = px(3)
#  )%>%
#  tab_style(
#    style = cell_text(align = "center"), 
#    locations = cells_column_labels(everything())
#  )


#summary_table
##gtsave(summary_table, "package_functions_summary.png")




  
  
####  
tab3.0 <- bdy%>%
  dplyr::select(age_grouping,bmirisk,Gender)%>%
  mutate(Age = paste(age_grouping))%>%
   mutate(whtrrisk = paste(bmirisk,"Body Mass Index")) %>%
  tbl_strata(strata=Gender,
             .tbl_fun = ~.x %>%
               tbl_summary(by=bmirisk,include = age_grouping,label = list(age_grouping ~ "Age Group"))%>%
               add_n(),
             .header = "**{strata}**",N={n}
            )
tab3.0
gt3.0 <- as_gt(tab3.0)
gt3.0
#gtsave(gt3.0,"BMIRISK.png")





billi1.0 <- ggplot(bdy,aes(bmirisk,Biiliac,fill=Gender))+
  geom_boxplot()+
  geom_signif(comparisons = list(c("Low Risk","High Risk")),annotation = "*")+
  theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))

billi1.10 <- billi1.0+
  labs(x="Risk Establshed by Body Mass Index",
       y= "Pelvic Bredth (cm)",
       fill="Gender")+
  theme(legend.position="bottom")

billi1.10
#ggsave("billi1.10.png", plot = billi1.10, width = 8, height = 6, dpi = 300)

t.test(data=bdy,Biiliac~bmirisk)

# whats next?
#####LDA#####
fit.18.0 <- lda(bmirisk~Biiliac,data=bdy)
pred1.0 <- predict(fit.18.0)
pred.class1 <- pred1.0$class

table(pred1.0$class,bdy$whtrrisk)
bdy2.0 <- bdy
bdy2.0$lda_class <- pred1.0$class
bdy2.0$LD1 <- pred1.0$x[,1] 
bdy2.0$pred_class <- pred1.0$class

bdy2.0 <- bdy2.0 %>%
  mutate(misclassified = ifelse(bmirisk != pred_class, "Yes", "No"))

ldamistakes.0 <- ggplot(bdy2.0, aes(misclassified,fill=Gender))+
  geom_bar(position="dodge")+theme_classic()+
  scale_fill_manual(values=c("#fc8d62","#66c2a5"))+
  labs(y="Count",x="Missclassified")+facet_wrap(vars(age_grouping))

ldamistakes.0 
#ggsave("ldamist.0.png", plot = ldamistakes.0, width = 8, height = 6, dpi = 300)

#get the formula
coef.0 <- fit.18.0$scaling

coef.0
cat("LD1 =",coef.0,"* Pelvic Bredth in cm")

##find percent accurate
actual_class.0 <- factor(bdy$whtrrisk, levels = levels(pred.class1))
actual_class.0 
cm.0 <- table(Predicted = pred.class1, Actual = actual_class.0)
cm.0

accuracy.0 <- sum(diag(cm.0)) / sum(cm.0) * 100
accuracy.0



