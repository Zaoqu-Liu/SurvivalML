library(gganatogram)
library(cowplot)

# generate organs in male
for (i in 1:nrow(hgMale_key)){
  organ = hgMale_key$organ[i]
  print(organ)
  p <- gganatogram(data=hgMale_key[i,],
              outline = T,
              fillOutline = "#ffffff",
              organism = "human",
              sex="male",
              fill="colour")+
    theme_void()
  cowplot::save_plot(paste(organ,".png",sep=""),
                     plot = p,base_height = 6,
                     base_width = 4
                     )
}

# generate organs which only in female 
hgFemale_key_specific = hgFemale_key[!hgFemale_key$organ %in% hgMale_key$organ,]
for (i in 1:nrow(hgFemale_key_specific)){
  organ = hgFemale_key_specific$organ[i]
  print(organ)
  p <- gganatogram(data=hgFemale_key_specific[i,],
                   outline = T,
                   fillOutline = "#ffffff",
                   organism = "human",
                   sex="female",
                   fill="colour")+
    theme_void()
  cowplot::save_plot(paste(organ,".png",sep=""),
                     plot = p,base_height = 6,
                     base_width = 4
  )
}

# use female breast instead of male breast
organ <- "breast"
p <- gganatogram(data=hgFemale_key[10,],
                 outline = T,
                 fillOutline = "#ffffff",
                 organism = "human",
                 sex="female",
                 fill="colour")+
  theme_void()
cowplot::save_plot(paste(organ,".png",sep=""),
                   plot = p,base_height = 6,
                   base_width = 4)

