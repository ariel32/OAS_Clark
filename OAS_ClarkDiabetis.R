library(tools)

setwd("diabetis_data/")
result = data.frame(id = character(0), slope = numeric(0))

for(file.src in dir(pattern = "*.rc4")) {
  x = file_path_sans_ext(file.src)
  if(grepl("d", x)) {d = 1; id = gsub("d", "", x)} else {d = 0; id = x}
  
  if (id %in% 1.1:7.1  & d == 0 ) file.baseline = "base/B1-7.rc4"
  if (id %in% 8.1:14.1 & d == 0 ) file.baseline = "base/B8-14.rc4"
  if (id %in% 15.1:20.1& d == 0 ) file.baseline = "base/B15-20.rc4"
  if (id %in% 21.1:22.1& d == 0 ) file.baseline = "base/B21-22.rc4"
  if (id %in% 1.1:10.1 & d == 1 ) file.baseline = "base/B1-10d.rc4"
  if (id %in% 11.1:20.1& d == 1 ) file.baseline = "base/B11-20d.rc4"
  if (id %in% 1.2:8.2  & d == 0 ) file.baseline = "base/B1.2-8.2.rc4"
  if (id %in% 1.2:10.2 & d == 1 ) file.baseline = "base/B1.2-10.2d.rc4"
  if (id %in% 1.3:2.3  & d == 0 ) file.baseline = "base/B1.3-2.3.rc4"
  if (id %in% 1.3:9.3  & d == 1 ) file.baseline = "base/B1.3-9.3d.rc4"
  if (id %in% 3.3:8.3  & d == 0 ) file.baseline = "base/B3.3-8.3.rc4"
  if (id %in% 9.2:13.2 & d == 0 ) file.baseline = "base/B9.2-13.2.rc4"
  if (id %in% 10.3:14.3& d == 1 ) file.baseline = "base/B10.3-14.3d.rc4"
  if (id %in% 11.2:19.2& d == 1 ) file.baseline = "base/B11.2-19.2d.rc4"
  if (id %in% 14.2:17.2& d == 0 ) file.baseline = "base/B14.2-17.2.rc4"
  
  baseline = read.table(file.baseline, skip = 7, dec = ",")[201:1000,2]
  dataline = read.table(file.src, skip = 7, dec = ",")[201:1000,2]
  
  targetline = dataline-baseline
  l = lm(targetline~seq(1,800))
  plot(dataline-baseline); abline(l)
  slope = as.numeric(coef(l)[2])
  
  if(d == 1) id = sprintf("%sd", id) else id = sprintf("%s_s", id)
  result = rbind(result, data.frame(id = id, slope = slope))
  
}

write.table(result, "../slope.csv", sep = ";", row.names = F)