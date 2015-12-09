library(tools)

setwd("diabetis_data/")
result = data.frame(id = character(0), slope = numeric(0))

for(file.src in dir(pattern = "*.rc4")) {
  id = file_path_sans_ext(file.src)
  
  if (id %in% 1:2  ) file.baseline = "base/B1-2.rc4"
  if (id %in% 3:4  ) file.baseline = "base/B3-4.rc4"
  if (id %in% 5:6  ) file.baseline = "base/B5-6.rc4"
  if (id %in% 7:7  ) file.baseline = "base/B7.rc4"
  if (id %in% 8:9  ) file.baseline = "base/B8-9.rc4"
  if (id %in% 10:10) file.baseline = "base/B10.rc4"
  if (id %in% 11:14) file.baseline = "base/B11-14.rc4"
  if (id %in% 15:16) file.baseline = "base/B15-16.rc4"
  if (id %in% 17:20) file.baseline = "base/B17-20.rc4"
  if (id %in% 21:22) file.baseline = "base/B21-22.rc4"
  if (id %in% 23:26) file.baseline = "base/B23-26.rc4"
  if (id %in% 27:28) file.baseline = "base/B27-28.rc4"
  if (id %in% 29:30) file.baseline = "base/B29-30.rc4"
  if (id %in% 31:32) file.baseline = "base/B31-32.rc4"
  if (id %in% 33:34) file.baseline = "base/B33-34.rc4"
  if (id %in% 35:36) file.baseline = "base/B35-36.rc4"
  if (id %in% 37:38) file.baseline = "base/B37-38.rc4"
  if (id %in% 39:40) file.baseline = "base/B39-40.rc4"
  
  baseline = read.table(file.baseline, skip = 7, dec = ",")[201:1000,2]
  dataline = read.table(file.src, skip = 7, dec = ",")[201:1000,2]
  
  targetline = dataline-baseline
  l = lm(targetline~seq(1,800))
  plot(dataline-baseline); abline(l)
  slope = as.numeric(coef(l)[2])
  
  result = rbind(result, data.frame(id = id, slope = slope))
  
}

write.table(result, "../slope.csv", sep = ";", row.names = F)