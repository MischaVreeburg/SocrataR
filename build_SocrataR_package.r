## debugging package, install
##build package

## load all the components
library(rkwarddev)
library(roxygen2)
#rm(list=ls())
#pathtoscripts <- 'c:/Users/public/Documents' # <- laptop
pathtoscripts <- 'g:' # <- usb
#pathtoscripts <- '/media/USB2' # <- usb


### sort of finished
source(file.path(pathtoscripts, 'local.git.rep/socratar/R/Socrata.r'))


### wip
# source(file.path(pathtoscripts, 'Glyco_analysis/Scripts/Glyco_analysis_KEGG_search.r'))



package.skeleton(name = 'SocrataR',
                 path = file.path(pathtoscripts, 'local.git.rep/socratar/release'),
                 force = TRUE,
                 namespace = TRUE,
                 code_files = c(file.path(pathtoscripts, 'local.git.rep/socratar/R/Socrata.r')
                                )
                )

roxygenize(package.dir = file.path(pathtoscripts, 'local.git.rep/socratar/'),
           roxygen.dir = file.path(pathtoscripts, 'local.git.rep/socratar/release'),
           copy.package = FALSE,
           roclets = c("namespace", "rd")
)

about.info <- rk.XML.about(
  name = "SocrataR",
  author = person(given = "M.D.", family = "Vreeburg",
                  email  ="m.d.vreeburg@gmail.com", role = c("aut", "cre")),
  about = list(
    desc = "SocrataR",
    version = "0.5",
    date = Sys.Date(),
    url = "http://cran.r-project.org/web/packages/SocrataR/index.html",
    Encoding = 'latin1',
    license = "LGPL",
    category = "",
    long.desc = "SocrataR provides an interface to the http://opendata/socrata.com datasets."),
    dependencies=list(
      R.min="2.10"),
    package = list(c(name='XML'),
                   c(name='RCurl'),
                   c(name='rjson')
                  )
  )


plugin.dir <- rk.plugin.skeleton(about = about.info,
                                 path = file.path(pathtoscripts, 'Eigen_R_Packages'),
                                 provides = NULL, # c("logic", "dialog"),
                                 scan = NULL, #c("var", "saveobj", "settings"),
                                 xml = NULL, #list(),
                                 js = NULL, #list(),
                                 pluginmap = NULL, #list(),
                                 rkh = NULL, #ls(),
                                 overwrite = TRUE,
                                 tests = FALSE,
                                 lazyLoad = TRUE,
                                 create = c("rkh", "desc"), # c("pmap", "xml", "js", "rkh", "desc"),
                                 suggest.required = FALSE,
                                 edit = FALSE,
                                 load = FALSE,
                                 show = FALSE,
                                 gen.info = FALSE,
                                 indent.by = "\t")


rk.build.plugin(plugin.dir, check = TRUE, install = F, R.libs = NULL)

rk.build.plugin(plugin.dir, check = TRUE, install = TRUE, R.libs = NULL)