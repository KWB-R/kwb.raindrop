
# Install some packages
install.packages('kwb.pkgbuild')


usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Michael Rustler",
               orcid = "0000-0003-0647-7726",
               url = "https://mrustl.de")


pkg <- list(name = "kwb.raindrop",
            title = "R Package for Optimisation Simulations for Rainwater Management Simulations Performed With Calculation Engine Provided by Tandler",
            desc  = "R Package for Optimisation Simulations for Rainwater Management Simulations Performed With Calculation Engine Provided by Tandler.")

kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("tutorial")

kwb.pkgbuild::use_ghactions()

kwb.pkgbuild::create_empty_branch_ghpages(pkg$name)
