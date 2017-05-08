
packages <- c("dplyr", "ckanr", "curl")

p <- installed.packages()

ifelse(length(packages[!(packages %in% row.names(p))]) > 0,
       install.packages(pkgs = packages[!(packages %in% row.names(p))]

ckanr_setup(url = "http://www.data.gov.au/")

packages <- group_show('business', as = 'table')$packages

data <- organization_show("ddd3e2d8-d0e8-4d43-a1e5-39984eb8e774", include_datasets = TRUE)

package_show('ddd3e2d8-d0e8-4d43-a1e5-39984eb8e774', as = 'table')

datasets <- ldply(data$packages, function(x) c(Dataset = x$title, ID =  x$id))

Company <- package_show(datasets %>% filter(Dataset == "ASIC - Company Dataset") %>% .[["ID"]], as = 'table')

r <- ckanr::resource_show("a1df019f-cb11-43e8-8d5d-0f830115c8a6")

r$name

# my.file.rename <- function(from, to) {
#   todir <- dirname(to)
#   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#   file.rename(from = from,  to = to)
# }
#
# my.file.rename(from = temp,
#                to = "//wvdb1devsql/ABRdata/ASIC/data")


#temp <- tempfile()
download.file(r$url, "company")

# d <- gsub(".*\\/(.*)\\.zip$", "\\1", r$url)

# system(paste0("move ",temp," \\wvdb1devsql\\ABRdata\\ASIC\\data"))
# unzip("company")
#
# data <- read.table(unzip("company"))
# unlink(temp)


data <- read.csv(unzip("company"), header=T, sep="\t", nrows=1000, stringsAsFactors = F)
DT::datatable(data)


url <- Company$resources %>% filter(name == "Company Dataset - Current") %>% .[["url"]]

script <- paste0('baseDir=//wvdb1devsql/ABRdata/ASIC; dataDir=$baseDir/data; mkdir -p $dataDir; dataUrl=', url, '; [[ -f $zip ]] || ( cd $dataDir; wget $dataUrl);')

write(script, file = "getASIC.sh")

shell("getASIC.sh", shell = "C:/Users/farrelp1/Documents/Git/bin/bash.exe")

curl(url, open = "r")

#package_list("")
