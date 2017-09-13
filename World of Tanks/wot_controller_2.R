### Wot Controller File 2 ###

ref1 <- read.csv("Z:\\Analytics Work\\Side Projects\\World of Tanks\\reference_table.csv")
names(ref1)[1] <- "tier"
ref1 <- ref1 %>% 
  mutate(tier = as.numeric(tier),
         battles = as.numeric(battles))

ref2 <- read.csv("Z:\\Analytics Work\\Side Projects\\World of Tanks\\reference_table_2.csv")
names(ref2)[1] <- "nation"
ref2 <- ref2 %>% 
  mutate(battles = as.numeric(battles))

ref3 <- read.csv("Z:\\Analytics Work\\Side Projects\\World of Tanks\\reference_table_3.csv")
names(ref3)[1] <- "tank_type"
ref3 <- ref3 %>% 
  mutate(battles = as.numeric(battles))


#### 