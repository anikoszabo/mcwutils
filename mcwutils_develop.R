library(devtools)

document()
test()
check()
build()

# only once, to set up GitHub
usethis::use_git()
usethis::use_github()
