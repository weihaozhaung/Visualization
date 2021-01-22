
library(reshape2)
library(extrafont)
font_import()
loadfonts(device = "win")
#cohort analysis
cohort.sum <- data %>%                         
  group_by(`User ID`) %>%                      
  mutate(first = min(Month)) %>%                
  group_by(first, Month) %>%                    
  summarise(users = n_distinct(`User ID`)) %>% 
  spread(Month, users)                         



cohort.chart <- melt(cohort.sum, id.vars = "first") %>% arrange(first)
colnames(cohort.chart) <- c('cohort', 'month', 'users')
cohort.chart[is.na(cohort.chart)] = 0

blues <- colorRampPalette(c("blue", "green"))
cohort.chart$cohort = as.factor((cohort.chart$cohort))
cohort.chart$cohort <- factor(cohort.chart$cohort, levels = rev(levels(cohort.chart$cohort)))

ggplot(cohort.chart, aes(x=month, y=users, group=cohort))+
  geom_area(aes(fill = cohort)) +
  scale_fill_manual(values = blues(nrow(cohort.sum))) +
  ggtitle('Total Users by Cohort')+
  labs(x = 'Month', y = 'Users')+
  scale_x_discrete(expand = c(0.03, 0.03))+
  theme_bw()+
  theme(text=element_text(size=10,  family="Segoe Print"))
#============================#
shiftrow <- function(v) {
  # put a vector in, strip off leading NA values, and place that amount at the end
  first_na_index <- min( which(!is.na(v)) )
  
  # return that bit to the end,  and pad with NAs.
  c(v[first_na_index:length(v)], rep(NA, first_na_index-1))
}
# create a new dataframe, with shifted rows (and keep the first one)
shifted <- data.frame(
  cohort = cohort.sum$first,
  t(apply( select(as.data.frame(cohort.sum), 2:ncol(cohort.sum)), # 2nd column to the end
           1, # for every row
           shiftrow ))
)
# and make column names readable
# first should be "cohort" and the rest Month-<number>, (padded)
colnames(shifted) <- c("cohort", paste0("Month-", str_pad(1:(ncol(shifted)-1),2,pad = "0")))
shifted_pct <- data.frame(
  cohort = shifted$cohort, # first column
  shifted[,1:nrow(shifted)+1] / shifted[["Month-01"]] # rest: divide by Month-1
)
plotdata_abs <- gather(shifted,     "cohort_age", "people"  ,2:ncol(shifted))
plotdata_pct <- gather(shifted_pct, "cohort_age", "percent" ,2:ncol(shifted_pct))

labelnames <- c( plotdata_abs$people[1:(ncol(shifted)-1)],
                 plotdata_pct$percent[(ncol(shifted)):(nrow(plotdata_pct))])

pretty_print <- function(n) {
  case_when( n <= 1  ~ sprintf("%1.0f %%", n*100),
             n >  1  ~ as.character(n),
             TRUE    ~ " ") 
}
# create the plot data
plotdata <- data.frame(
  cohort     = plotdata_pct$cohort,
  cohort_age = plotdata_pct$cohort_age,
  percentage = plotdata_pct$percent,
  label      = pretty_print(labelnames)
)
plotdata[which(plotdata$percentage == 1), "percentage"] <- 0



# library(showtext)
# font_add_google("Montserrat", "Montserrat")
# font_add_google("Roboto", "Roboto")

ggplot(plotdata, aes(x = cohort_age, y = reorder(cohort, desc(cohort)))) +
  geom_raster(aes(fill = percentage)) +
  scale_fill_continuous(guide = FALSE) + # no legend
  geom_text(aes(label = label), color = "white", family = 'Comic Sans MS', size = 3) +
  xlab("Period") + ylab("Month of First Purchase") + 
  ggtitle(paste("Retention table"))+theme_bw()+
  theme(text=element_text(size=9,  family="Segoe Print"))

